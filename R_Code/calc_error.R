calc_mse <- function(predicted, factual)
{
    result = sum((predicted - factual)^2) / length(predicted)
    return(result)
}