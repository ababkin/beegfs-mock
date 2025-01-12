# Configuration
The mountebank will be using imposters defined in the `imposters/imposters.ejs` file.

## set up ssh keys
```
mkdir -p ssh_keys

# use your own existing key
cp ~/.ssh/id_rsa.pub ssh_keys/

# or generate a new set of keys and use it with the -i option later
```


# Start up
```
# build docker image
./scripts/build-docker.sh

# start docker compose
docker compose up

```

# Usage
```
# ssh into container
ssh -o StrictHostKeyChecking=no -p 2222 root@localhost

# run beegfs-ctl getquota
beegfs-ctl getquota --uid --all --mount=/project

# set quota
beegfs-ctl setquota --uid=1000 --sizelimit=1T --mount=/project

# query actual imposter
curl http://localhost:2525/imposters/8080 | jq '.requests'


# check imposters status
curl http://localhost:2525/imposters


```

ren tests
```
cabal test --enable-optimization=0 --disable-documentation
```
