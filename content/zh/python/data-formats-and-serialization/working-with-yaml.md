---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:23.929438-07:00
description: "YAML\u662F\u6307\u201CYAML Ain't Markup Language\u201D\uFF0C\u662F\u4E00\
  \u79CD\u5BF9\u4EBA\u7C7B\u53EF\u8BFB\u7684\u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\
  \u3002\u7A0B\u5E8F\u5458\u4F7F\u7528YAML\u8FDB\u884C\u914D\u7F6E\u6587\u4EF6\u7F16\
  \u5199\u3001\u8FDB\u7A0B\u95F4\u4FE1\u606F\u4F20\u9012\u548C\u6570\u636E\u5B58\u50A8\
  \uFF0C\u56E0\u4E3A\u4E0EXML\u6216JSON\u7B49\u5176\u4ED6\u683C\u5F0F\u76F8\u6BD4\uFF0C\
  YAML\u7684\u8BED\u6CD5\u7B80\u5355\uFF0C\u6613\u8BFB\u6027\u5F3A\u3002"
lastmod: '2024-02-25T18:49:44.910499-07:00'
model: gpt-4-0125-preview
summary: "YAML\u662F\u6307\u201CYAML Ain't Markup Language\u201D\uFF0C\u662F\u4E00\
  \u79CD\u5BF9\u4EBA\u7C7B\u53EF\u8BFB\u7684\u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\
  \u3002\u7A0B\u5E8F\u5458\u4F7F\u7528YAML\u8FDB\u884C\u914D\u7F6E\u6587\u4EF6\u7F16\
  \u5199\u3001\u8FDB\u7A0B\u95F4\u4FE1\u606F\u4F20\u9012\u548C\u6570\u636E\u5B58\u50A8\
  \uFF0C\u56E0\u4E3A\u4E0EXML\u6216JSON\u7B49\u5176\u4ED6\u683C\u5F0F\u76F8\u6BD4\uFF0C\
  YAML\u7684\u8BED\u6CD5\u7B80\u5355\uFF0C\u6613\u8BFB\u6027\u5F3A\u3002"
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
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
