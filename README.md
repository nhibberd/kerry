kerry
=====

[Packer](https://www.packer.io/) configuration and serialization.



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
