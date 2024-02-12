---
title:                "使用YAML工作"
aliases: - /zh/python/working-with-yaml.md
date:                  2024-02-03T19:26:23.929438-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用YAML工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么与为什么？
YAML是指“YAML Ain't Markup Language”，是一种对人类可读的数据序列化格式。程序员使用YAML进行配置文件编写、进程间信息传递和数据存储，因为与XML或JSON等其他格式相比，YAML的语法简单，易读性强。

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
