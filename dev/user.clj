(ns user)

(defn dev
  "Load and switch to the 'dev' namespace."
  []
  (load "dev")
  (require 'dev)
  (in-ns 'dev))
