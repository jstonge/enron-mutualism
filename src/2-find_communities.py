
import sys, os
from collections import Counter
from pathlib import Path

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

import graph_tool.all as gt

ROOT_DIR = Path("socsemics-enron.Rproj").resolve().parent.parent
os.chdir(ROOT_DIR)

# ------------------------------------------------------------------------------
# HELPER FUNS
# ------------------------------------------------------------------------------

def build_weighted_graph(edges_df, directed=False):
  g = gt.Graph(directed=directed)
  w = g.new_edge_property("int")
  g.edge_properties["weight"] = w
  
  for i in range(len(edges_df)):
      e = g.add_edge(edges_df.iloc[i,0],edges_df.iloc[i,1])
      w[e] = edges_df.iloc[i,2]
  return g

def add_labs():
  nodes_df['labs'] = nodes_df.name.str.replace("@enron.com", "", regex=True)
  labs = g.new_vp("string")
  g.vertex_properties["label"] = labs

  for i in range(len(nodes_df)):
    v = g.vertex(nodes_df.loc[i,'idx'])
    labs[v] = nodes_df.loc[i,"labs"]
  return g

def plot_hierarchical_high_res(g, state):
  state.draw(
    vertex_color = "black", vertex_font_size = 11, 
    vertex_text = g.vp.label, vertex_text_position = "centered",
    edge_color=g.ep.weight, 
    ecmap=(plt.cm.Greys, .6), eorder=g.ep.weight, 
    edge_pen_width=gt.prop_to_size(g.ep.weight, 2, 8, power=1), 
    edge_gradient=[], 
    output_size = (1000, 1000), 
    output = str(ROOT_DIR/"figs"/"hsbm.svg")
    )

def plot_triptych(g, state):
  deg = g.degree_property_map("total")
  fig, ax = plt.subplots(1, 3, figsize=(13, 7))
  state.draw(edge_pen_width=gt.prop_to_size(g.ep.weight, 0.001, 0.05),
                 edge_color=g.ep.weight, ecmap=(plt.cm.Greys, .5),
                 eorder=g.ep.weight,
                 edge_gradient=[],mplfig=ax[0])
  
  state.levels[0].draw(vertex_color="black",
                           vertex_text=state.levels[0].get_blocks(),
                           vertex_size=gt.prop_to_size(deg,0.5,1.5) ,
                           mplfig=ax[1])
  
  dense_mat = state.levels[0].get_matrix().todense()
  p_mat = ax[2].imshow(dense_mat, cmap=plt.cm.Greys)
  
  cbar = plt.colorbar(p_mat, shrink=0.55, label='Weight', ax=ax[2])
  
  ax[2].set_xticks(np.arange(len(dense_mat)))
  ax[2].set_yticks(np.arange(len(dense_mat)))
  
  ax[0].axis('off')
  ax[1].axis('off')
  
  fig.savefig(ROOT_DIR/"figs"/"fig1.png",  dpi=400)

def write_state_to_disk(state, nodes_df, fname):
  
  levels = state.get_levels() # donne les niveaux
  r = levels[0].get_blocks()
  
  idc = [] 
  c = [] 
  for i in range(len(list(g.vertices()))):
      idc.append(i)
      c.append(r[i])
  
  
  communautes = pd.DataFrame({"idx":idc, "communaute":c})\
                  .merge(nodes_df, how="left")\
                  .rename(columns={"label":"nom"})\
                  .assign(idx = lambda x: x.idx + 1)

  communautes.to_csv(ROOT_DIR/"output"/f"{fname}.csv",index=False)

# ==============================================================================


edges_df = pd.read_csv(ROOT_DIR/'output'/'edges_df.csv')
nodes_df = pd.read_csv(ROOT_DIR/'output'/'nodes_df.csv')

edges_df["from"] = edges_df["from"] - 1
edges_df["to"] = edges_df["to"] - 1
nodes_df['idx'] = nodes_df['idx'] - 1

g = build_weighted_graph(edges_df)
g = add_labs()

gt.seed_rng(43)

state = gt.minimize_nested_blockmodel_dl(g)

S = state.entropy()


# binomial dcHSBM --------------------------------------------------------


state_binomial = gt.minimize_nested_blockmodel_dl(g, 
                                         state_args=dict(recs=[g.ep.weight],
                                                         rec_types=["discrete-binomial"]))
S_binomial = state_binomial.entropy()


# geometric dcHSBM --------------------------------------------------------


state_geo = gt.minimize_nested_blockmodel_dl(g, 
                                         state_args=dict(recs=[g.ep.weight],
                                                         rec_types=["discrete-geometric"]))

S_geo = state_geo.entropy()


# model validation --------------------------------------------------------


print(f"Entropy vanilla: {S}")      # although lower entropy, we have theoretical reasons why to keep the G weighted
print(f"Entropy Binomial: {S_binomial}")
print(f"Entropy Geo: {S_geo}")


# Plotting ----------------------------------------------------------------


plt.style.use("seaborn-white")
plt.switch_backend("cairo")
plot_hierarchical_high_res(g, state_geo)
plot_triptych(g, state_geo)


# Write to disk -----------------------------------------------------


write_state_to_disk(state_geo, nodes_df, "communautes_geoSBM")
