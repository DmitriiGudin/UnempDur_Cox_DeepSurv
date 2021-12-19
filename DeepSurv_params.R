#EXPERIMENTS WITH THE FULL DATASET

model <- deepsurv(data=UnempDur, num_nodes=c(32L,32L,16L,8L), activation='relu', dropout=0.2, epochs = 100L, batch_size = 256L, verbose=TRUE)
p <- predict(model, type='risk')
cindex(risk=p, truth = UnempDur[, 'time'])
#0.636603

model <- deepsurv(data=UnempDur, num_nodes=c(32L,32L,16L,8L), activation='relu', dropout=0.2, epochs = 200L, batch_size = 256L, verbose=TRUE)
p <- predict(model, type='risk')
cindex(risk=p, truth = UnempDur[, 'time'])
#0.6427761

model <- deepsurv(data=UnempDur, num_nodes=c(32L,16L,8L), activation='relu', dropout=0.2, epochs = 200L, batch_size = 256L, verbose=TRUE)
p <- predict(model, type='risk')
cindex(risk=p, truth = UnempDur[, 'time'])
#0.6426914

model <- deepsurv(data=UnempDur, num_nodes=c(256L,128L), activation='relu', dropout=0.2, epochs = 200L, batch_size = 256L, verbose=TRUE)
p <- predict(model, type='risk')
cindex(risk=p, truth = UnempDur[, 'time'])
#0.6411895

model <- deepsurv(data=UnempDur, num_nodes=c(16L,8L,4L,4L), activation='relu', dropout=0.2, epochs = 200L, batch_size = 256L, verbose=TRUE)
p <- predict(model, type='risk')
cindex(risk=p, truth = UnempDur[, 'time'])
#0.6309746

model <- deepsurv(data=UnempDur, num_nodes=c(8L,4L,2L), activation='relu', dropout=0.2, epochs = 200L, batch_size = 256L, verbose=TRUE)
p <- predict(model, type='risk')
cindex(risk=p, truth = UnempDur[, 'time'])
#0.6253917

model <- deepsurv(data=UnempDur, num_nodes=c(4L,4L), activation='relu', dropout=0.2, epochs = 200L, batch_size = 256L, verbose=TRUE)
p <- predict(model, type='risk')
cindex(risk=p, truth = UnempDur[, 'time'])
#0.6380277

model <- deepsurv(data=UnempDur, num_nodes=c(128L), activation='relu', dropout=0.2, epochs = 200L, batch_size = 256L, verbose=TRUE)
p <- predict(model, type='risk')
cindex(risk=p, truth = UnempDur[, 'time'])
#0.6444061

model <- deepsurv(data=UnempDur, num_nodes=c(4L), activation='relu', dropout=0.2, epochs = 200L, batch_size = 256L, verbose=TRUE)
p <- predict(model, type='risk')
cindex(risk=p, truth = UnempDur[, 'time'])
#0.641557

model <- deepsurv(data=UnempDur, num_nodes=c(1L), activation='relu', dropout=0.2, epochs = 200L, batch_size = 256L, verbose=TRUE)
p <- predict(model, type='risk')
cindex(risk=p, truth = UnempDur[, 'time'])
#0.5

model <- deepsurv(data=UnempDur, num_nodes=c(2L,2L,2L,2L), activation='relu', dropout=0.2, epochs = 200L, batch_size = 256L, verbose=TRUE)
p <- predict(model, type='risk')
cindex(risk=p, truth = UnempDur[, 'time'])
#0.6344102