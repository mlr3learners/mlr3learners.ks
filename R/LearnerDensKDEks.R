#' @title Density KS Kernel Learner
#'
#' @name mlr_learners_dens.kdeKS
#'
#' @description
#' A [mlr3::LearnerDens] implementing kdeKS from package
#'   \CRANpkg{ks}.
#' Calls [ks::kde()].
#'
#' @templateVar id dens.kdeKS
#' @template section_dictionary_learner
#'
#' @template seealso_learner
#' @template example
#' @export
LearnerDensKDEks = R6Class("LearnerDensKDEks",
  inherit = LearnerDens,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamDbl$new(id = "h", lower = 0, tags = "train"),
          ParamUty$new(id = "H", tags = "train"),
          ParamUty$new(id = "gridsize", tags = "train"),
          ParamUty$new(id = "gridtype", tags = "train"),
          ParamDbl$new(id = "xmin", tags = "train"),
          ParamDbl$new(id = "xmax", tags = "train"),
          ParamDbl$new(id = "supp", default = 3.7, tags = "train"),
          ParamDbl$new(id = "binned", tags = "train"),
          ParamUty$new(id = "bgridsize", tags = "train"),
          ParamLgl$new(id = "positive", default = FALSE, tags = "train"),
          ParamUty$new(id = "adj.positive", tags = "train"),
          ParamUty$new(id = "w", tags = "train"),
          ParamLgl$new(id = "compute.cont", default = TRUE, tags = "train"),
          ParamLgl$new(id = "approx.cont", default = TRUE, tags = "train"),
          ParamLgl$new(id = "unit.interval", default = FALSE, tags = "train"),
          ParamLgl$new(id = "verbose", default = FALSE, tags = "train"),
          ParamLgl$new(id = "zero.flag", default = TRUE, tags = "train")
        )
      )

      super$initialize(
        id = "dens.kdeKS",
        packages = "ks",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = "pdf",
        param_set = ps,
        man = "mlr3learners.ks::mlr_learners_dens.kdeKS"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tag = "train")

      data = task$truth()

      pdf <- function(x1) {
      }
      body(pdf) <- substitute({
        invoke(ks::kde, x = data, eval.points = x1, .args = pars)$estimate
      })

      distr6::Distribution$new(
        name = "ks KDE",
        short_name = "ksKDE",
        pdf = pdf)
    },

    .predict = function(task) {
      PredictionDens$new(task = task, pdf = self$model$pdf(task$truth()))
    }
  )
)
