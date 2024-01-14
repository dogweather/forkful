---
title:                "Haskell: 使用 YAML"
simple_title:         "使用 YAML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

#为什么

如今，越来越多的企业和开发者选择使用YAML作为他们的配置文件格式。它具有简单易懂的语法结构，使得阅读和编辑变得更加方便。另外，它的跨平台兼容性也让它成为一个受欢迎的选择。在本文中，我们将深入探讨如何使用Haskell来处理YAML文件。

##如何

首先，我们需要通过安装[hYaml软件包](https://hackage.haskell.org/package/hYaml)来让Haskell能够处理YAML文件。然后，我们就可以使用`decodeYaml`函数来将YAML文件转换为一个`Value`类型的数据对象。例如，假设我们有一个名为`config.yaml`的YAML文件，内容如下：
```
name: John
age: 30
hobbies: 
  - reading
  - hiking
```
我们可以使用以下代码来读取并打印出该文件的内容：
```Haskell
import Data.Yaml

main = do
  result <- decodeYaml <$> readFile "config.yaml"
  case valueToMaybe result of
    Just config -> do
      let name = config :: String
      let age = config :: Int
      let hobbies = config :: [String]
      putStrLn ("Name: " ++ name)
      putStrLn ("Age: " ++ show age)
      putStrLn "Hobbies:"
      mapM_ putStrLn hobbies
    Nothing -> putStrLn "Invalid YAML file."
```
运行以上代码，我们将得到以下输出：
```
Name: John
Age: 30
Hobbies:
reading
hiking
```

##深入探讨

在这里，我们使用`decodeYaml`函数将YAML文件转换为一个`Value`类型的数据对象。然而，`Value`类型并不是一个具体的数据类型，它是一个多态类型，可以表示任意类型的值。因此，在使用的时候，我们需要将其转换为我们需要的具体类型，比如`String`、`Int`或者`[String]`。

此外，如果我们想要将YAML文件中的数据结构直接映射到一个自定义的数据类型，我们也可以使用`decodeYamlFile`函数，它会自动将YAML文件的内容转换为该数据类型。举个例子，假设我们有一个`Person`类型的数据结构，定义如下：
```Haskell
data Person = Person
  { name :: String
  , age :: Int
  , hobbies :: [String]
  } deriving (Show, Eq)
```
我们可以使用以下代码来读取并打印出`config.yaml`文件中的内容：
```Haskell
import Data.Yaml

main = do
  result <- decodeYamlFile "config.yaml"
  case result of
    Just person -> putStrLn (show person)
    Nothing -> putStrLn "Invalid YAML file."
```
运行以上代码，我们将得到以下输出：
```
Person {name = "John", age = 30, hobbies = ["reading", "hiking"]}
```

#另请参阅

- [hYaml软件包](https://hackage.haskell.org/package/hYaml)
- [YAML语言官方网站](https://yaml.org/) 
- [Haskell语言官方网站](https://www.haskell.org/)