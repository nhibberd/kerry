kerry
=====

[Packer](https://www.packer.io/) configuration and serialization.

[![Hackage][hackage-shield]][hackage]



## Example

```haskell

import           Kerry

write :: FilePath -> IO ()
write path =
  writeFile path (renderPacker example)

example :: Packer
example =
  Packer {
      variables = [
          UserVariable "name" "example-packer"
        ]
    , builders = [
          Builder (AmazonEBSBuilder $ aws builder) Nothing ssh
        ]
    , provisioners = []
    , postProcessors = []
    }

ssh :: Communicator
ssh =
  SSH $ defaultSSHCommunicator "ec2-user"

aws :: a -> AWS a
aws builder =
  AWS {
      awsRegion = "us-west-2"
    , awsCredentials = EnvironmentVariables
    , awsBuilder = builder
    }

builder :: EBS
builder =
 ebs
   "test"
   (SourceAmiId "ami-fred")
   "m4.xlarge"

```





 [hackage]: http://hackage.haskell.org/package/kerry
 [hackage-shield]: https://img.shields.io/badge/hackage-v0.0.1-blue.svg
