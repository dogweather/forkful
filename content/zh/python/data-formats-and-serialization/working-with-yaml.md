---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:23.929438-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Python\u4E2D\u8BFB\u5199YAML\u901A\
  \u5E38\u9700\u8981\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\uFF0C`PyYAML`\u662F\u6700\
  \u6D41\u884C\u7684\u9009\u62E9\u3002\u9996\u5148\uFF0C\u4F60\u9700\u8981\u901A\u8FC7\
  \u8FD0\u884C`pip install PyYAML`\u6765\u5B89\u88C5PyYAML\u3002 **\u793A\u4F8B\uFF1A\
  \u5199\u5165YAML\u6587\u4EF6**."
lastmod: '2024-04-05T21:53:47.630991-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

## 如何操作：
在Python中读写YAML通常需要使用第三方库，`PyYAML`是最流行的选择。首先，你需要通过运行`pip install PyYAML`来安装PyYAML。

**示例：写入YAML文件**

```python
import yaml

data = {'a list': [1, 42, 3.141, 1337, 'help', u'€'],
        'a string': 'boo!',
        'another dict': {'foo': 'bar', 'key': 'value', 'the answer': 42}}

with open('example.yaml', 'w') as f:
    yaml.dump(data, f, default_flow_style=False)

# 这将创建`example.yaml`文件，并以YAML格式结构化数据。
```

**示例：从YAML文件读取**

```python
import yaml

with open('example.yaml', 'r') as f:
    data_loaded = yaml.safe_load(f)

print(data_loaded)

# 输出: 
# {'a list': [1, 42, 3.141, 1337, 'help', '€'],
#  'a string': 'boo!',
#  'another dict': {'foo': 'bar', 'key': 'value', 'the answer': 42}}
```

**使用YAML进行配置**

许多程序员使用YAML来管理应用程序配置。以下是如何结构化配置文件并读取它的示例：

config.yaml:
```yaml
database:
  host: localhost
  port: 5432
  username: admin
  password: secret
```

在Python中读取配置文件：
```python
import yaml

with open('config.yaml', 'r') as f:
    config = yaml.safe_load(f)

print(config['database']['host'])  # 输出: localhost
```

**处理复杂结构**

对于复杂的结构，PyYAML允许你定义自定义的Python对象。但是，确保使用`safe_load`以避免执行任意的函数或对象，从而保证安全做法。

```python
import yaml

# 定义一个Python对象
class Example:
    def __init__(self, value):
        self.value = value

# 自定义构造函数
def constructor_example(loader, node):
    value = loader.construct_scalar(node)
    return Example(value)

# 为标签"!example"添加构造函数
yaml.add_constructor('!example', constructor_example)

yaml_str = "!example 'data'"
loaded = yaml.load(yaml_str, Loader=yaml.FullLoader)

print(loaded.value)  # 输出: data
```

在这个片段中，`!example`是一个用于从YAML字符串实例化包含'value'数据的`Example`对象的自定义标签。像这样的自定义加载器扩展了PyYAML的灵活性，使其能够处理更复杂的数据结构和类型。
