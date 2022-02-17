import pandas as pd
from scipy import stats

data = pd.read_csv('medical_data.csv')

def perform_tests(data):
    categorial_list = list(data.select_dtypes(include=['int64']).columns)
    numerical_list = list(data.select_dtypes(include=['float64']).columns)

    df0 = data[data['death'] == 0]
    df1 = data[data['death'] == 1]

    # test for normality on numerical variables
    normal_var_df0 = []
    normal_var_df1 = []
    spr = []
    for i in range(len(numerical_list)):
        r1 , p1 = stats.shapiro(df0[numerical_list[i]])
        r2 , p2 = stats.shapiro(df1[numerical_list[i]])
        spr.append((numerical_list[i], round(p1,3), round(p2,3)))
        if p1 > 0.05:
            normal_var_df0.append((numerical_list[i]))
        if p2 > 0.05:
            normal_var_df1.append((numerical_list[i]))

    normal_var = list(set(normal_var_df0) & set(normal_var_df1))
    non_norm_var = list(set(numerical_list) - set(normal_var))

    # Mann Whitney
    mwr = []
    for i in range(len(non_norm_var)):
        r, p = stats.mannwhitneyu(df0[non_norm_var[i]], df1[non_norm_var[i]])
        mwr.append((non_norm_var[i],round(p,3)))

    # T-test
    ttr = []
    for i in range(len(normal_var)):
        r, p = stats.ttest_ind(df0[normal_var[i]], df1[normal_var[i]], equal_var = False)
        ttr.append((normal_var[i],round(p,3)))

    # Chi-squared
    csr = []
    for i in range(len(categorial_list)):
        contigency= pd.crosstab(data['death'], data[categorial_list[i]])
        r, p, dof, expctd = stats.chi2_contingency(contigency)
        csr.append((categorial_list[i],round(p)))
        
    return {
        'mann_whitney': mwr,
        'ttest': ttr,
        'chi_square': csr,
        'shapiro_wilk': spr
    }

perform_tests(data)
