# Projecoes para as eleições 2022

Projeções para o resultado das eleições de 2022 para prsidente da republica usando resultados de pesquisas eleitorais disponibilizados no [GitHub do Jornal Nexo](https://github.com/Nexo-Dados/pesquisas-presidenciais-2022/). A ideia dessa modelagem surgiu a partir de um [Tweet](https://twitter.com/gzanlorenssi/status/1573083879879090176) do [Gabriel Lorenssi](https://twitter.com/gzanlorenssi) e do meu interesse em acompanhar das pesquisas agregado os resultados e prevendo até o dia da eleição.

Vou considerar apenas os institutos considerados neutros segundo o site [Poolidata](http://pollingdata.com.br/) que se baseia na avaliação do governo. Institutos que se distanciam demais da média dos demais institutos sao cnsiderads favoráveis ou desfavoráveis ao governo. Usei apenas os classificados como neutros (Ipec, Datafolha, Real Time Big Data, Sensus,
FSB, Quaest e Ideia Big Data).

## Resultados

### Densidade da proporção de votos válidos para cada candidato no dia da eleição  

![][id2]

### Violin plot dos votos válidos para cada candidato no dia da eleição  

![][id3]

### Estimativas para o dia da eleição

```
Candidato   Prop     LI     LS Prob_vitoria_1o_turno
  <ord>      <dbl>  <dbl>  <dbl>                 <dbl>
1 Lula      0.505  0.473  0.537                  0.641
2 Bolsonaro 0.355  0.328  0.385                  0    
3 Ciro      0.0633 0.0490 0.0798                 0    
4 Tebet     0.0553 0.0350 0.0798                 0    
5 Outros    0.0194 0.0116 0.0335                 0     
```

### Projeções para a proporção de votos em cada candidato até o dia da eleição do primeiro turno.

![][id1]


## Descrição do método

O modelo proposto utiliza as estimativas para a proporção de votos de cada candidato como desfecho ou variável resposta, e prevê para cada candidato co serão os próximos dias até a data da eleição.

O modelo estatístico escolhido foi um modelo dinâmico de crescimento linear considerando dados diários e uma transformação logística para as proporções. Seja $P_{k,t}$ a proporção de votos para o candidato $k$ no dia $t$ do insttuto $i$. 
$$Y_{k,t,i} = \log\left(\frac{P_{k,t,i}}{1-P_{k,t,i}}\right) \sim Normal( \mu_{k,t,i}, \sigma_k^2)$$
onde $\forall k$ assumimos que a méda de $Y_{k,t,i}$ assume um valor médio para cada candidato, e comum no tempo e entre institutos, adicionado por dois efeitos aleatórios. Um não estruturado para capturar a heterogenedade cada instituto, e outro, agora estrutura, para descrever e incluir a dependência temporal entre estimativas.
$$\mu_{k,t,i} = \mu_k + \alpha_{k,t} + \beta_{k,i},$$
$$\mu_{k} \sim N(0, V \to \infty),$$
$$\alpha_{k,t} \sim AR(2, \tau^2_\alpha), \quad , \quad \tau_\alpha^2>0,$$
$$\beta_{k,i} \sim Normal(0, \tau^2_\beta\), \quad \tau_\beta^2>0, $$

O modelo se completa com distribuições a priori para os parâmetros. Foram utilizadas prioris vagas, isto é, pouco informativas e default do pacote INLA (https://www.r-inla.org/).

Uma vez estimados os parâmetros, a distribuição preditiva é acessada gerando estimativas para todos os dias desde o primeiro dia que se tem alguma pesquisa até o dia da eleição. Para dias anteriores ao dia da última pesquisa disponível no banco temos as estimativas ajustadas, e entre o último dia com algum resultado de pesquisa eleitoral até a data da eleição temos as projeções diárias de cada candidato sob esse modelo.

Os candidatos considerados foram Lula, Bolsonaro, Ciro, e Tebet. Os demais candidatos foram agrupados em Outros, e brancos nulos e indecisos foram chamados de BNI. Outros e BNI foram tratados como se fossem candidatos independentes.

### Calculando a projeção de votos no dia da eleição

As projeções das estimativas de cada candidato foram geradas, e para o dia da eleição amostras da distribuição preditiva a proporção de votos de cada candidato foi gerada, e a proporção de votos válidos foi calculada removendo os brancos, nulos e indecisos e recalculando os totais.

Com a amostra da distribuição preditiva da proporção de votos, podemos calcular a probabilidade de vitória já no primeiro turno para cada candidato. Essa etapa é feita usando integração de Monte Carlo.

Os códigos estão disponíveis [aqui](R/projections.r).


## Limitações

Existem muitas limitações, algumas delas:

* O modelo não considera a abstenção que nas últimas eleições ficou acima de 20%.
* Não assume nenhuma mudança de cenário, apenas acompanha a tendencia de crescimento (linear) mais recente.
* Uso de modelos gaussianos em transfrmações dos dados poderia ser aprimorado usando por exemplo a distribuição Dirichet. Isso é importante pois como no formato atual cada candidato é tratado separadamente, teoricamente é possível que a soma das proporções passe de 100%. No passo de Monte Carlo (isso é corrigido ao remover a categoria BNI e recalcular as proporções).
* Todos institutos de pesquisas tiveram o mesmo peso.
* Certamente exstem outras, postarei aqui se lembrar de outra.

[id1]: figs/projecoes.png ""
[id2]: figs/density.png ""
[id3]: figs/violin.png ""
