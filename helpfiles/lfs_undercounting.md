The measure proposed in this approach aims to estimate undercounting in survey data related to migration. It utilizes two methods to quantify undercounting: expected missing migration counts (*U*) and a standardized measure (*S*) that expresses undercounting proportionally to the total migration counts.

First, the population size (*N*) is estimated by summing all the weights assigned to survey respondents. Then, the proportion of migrating individuals (*R*) is estimated by dividing the estimated migration counts (*M*) by the estimated population size minus the estimated population with missing migration data (*V*):

$$ R = \frac{M}{N - V} $$

The expected missing migration counts (*U*) are derived by multiplying the migration rate (*R*) by the size of the population group with missing migration data (*V*):

$$ U = R \cdot V $$

Instead of using *U* directly as the undercounting measure, we propose a standardized approach where undercounting is expressed proportionally to the total migration counts. This is done by calculating *S*, which represents the fraction of undercounting (*U*) in relation to the total migrating population (observed + "imputed"):

$$ S = \frac{U}{U + M} $$

The formula for *S* simplifies to:

$$ S = \frac{V}{N} $$

where *V* represents the size of the population group with missing migration data, and *N* is the total estimated population size.

However, there are several problems with the measure. The assumption that the missing migration data can be accurately estimated based on the size of the population group with missing data may overlook variations in missing data patterns or causes of the data missingness. Additionally, the measure relies heavily on the accuracy of migration counts, the representativeness of the survey sample, and the validity of assigned weights.
