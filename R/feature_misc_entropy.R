#' @importFrom kdpee kdpee
calculateEntropyFeatures = function(feat.object, control, ...) {
  assertClass(feat.object, "FeatureObject")
  if (missing(control))
    control = list()
  assertList(control)
  measureTime(expression({
    X = extractFeatures(feat.object)
    Y = matrix(extractObjective(feat.object), ncol=1)
    Hyhat = kdpee(Y)
    Hxhat = kdpee(X) 
    Hxyhat = kdpee(cbind(Y, X))
    # Entropic significance of d-th order
    xi_d = (Hxhat + Hyhat - Hxyhat) / Hyhat
    list(entropy.Hy = Hyhat, entropy.xi_d = xi_d)
  }), "entropy")
}
