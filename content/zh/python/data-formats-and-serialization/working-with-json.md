---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:07.498796-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Python \u5185\u7F6E\u7684 `json` \u5E93\
  \u7B80\u5316\u4E86\u7F16\u7801\uFF08\u5C06 Python \u5BF9\u8C61\u8F6C\u6362\u4E3A\
  \ JSON\uFF09\u548C\u89E3\u7801\uFF08\u5C06 JSON \u8F6C\u6362\u4E3A Python \u5BF9\
  \u8C61\uFF09\u7684\u8FC7\u7A0B\u3002\u4EE5\u4E0B\u662F\u60A8\u5982\u4F55\u4F7F\u7528\
  \u5B83\u7684\u65B9\u6CD5\uFF1A."
lastmod: '2024-03-13T22:44:47.279883-06:00'
model: gpt-4-0125-preview
summary: "Python \u5185\u7F6E\u7684 `json` \u5E93\u7B80\u5316\u4E86\u7F16\u7801\uFF08\
  \u5C06 Python \u5BF9\u8C61\u8F6C\u6362\u4E3A JSON\uFF09\u548C\u89E3\u7801\uFF08\u5C06\
  \ JSON \u8F6C\u6362\u4E3A Python \u5BF9\u8C61\uFF09\u7684\u8FC7\u7A0B\u3002\u4EE5\
  \u4E0B\u662F\u60A8\u5982\u4F55\u4F7F\u7528\u5B83\u7684\u65B9\u6CD5\uFF1A\n"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
weight: 38
---

## 如何操作：
Python 内置的 `json` 库简化了编码（将 Python 对象转换为 JSON）和解码（将 JSON 转换为 Python 对象）的过程。以下是您如何使用它的方法：

### 将 Python 对象编码为 JSON：
```python
import json

data = {
    "name": "John Doe",
    "age": 30,
    "isEmployee": True,
    "addresses": [
        {"city": "New York", "zipCode": "10001"},
        {"city": "San Francisco", "zipCode": "94016"}
    ]
}

json_string = json.dumps(data, indent=4)
print(json_string)
```

**输出：**

```json
{
    "name": "John Doe",
    "age": 30,
    "isEmployee": true,
    "addresses": [
        {
            "city": "New York",
            "zipCode": "10001"
        },
        {
            "city": "San Francisco",
            "zipCode": "94016"
        }
    ]
}
```

### 将 JSON 解码为 Python 对象：
```python
json_string = '''
{
    "name": "John Doe",
    "age": 30,
    "isEmployee": true,
    "addresses": [
        {
            "city": "New York",
            "zipCode": "10001"
        },
        {
            "city": "San Francisco",
            "zipCode": "94016"
        }
    ]
}
'''

data = json.loads(json_string)
print(data)
```

**输出：**

```python
{
    'name': 'John Doe', 
    'age': 30, 
    'isEmployee': True, 
    'addresses': [
        {'city': 'New York', 'zipCode': '10001'}, 
        {'city': 'San Francisco', 'zipCode': '94016'}
    ]
}
```

### 使用第三方库：
对于复杂的 JSON 处理，比如架构验证或直接从URL解析JSON文件，例如使用 `requests` 库进行HTTP请求和使用 `jsonschema` 库进行验证，可能会很有帮助。

#### 使用 `requests` 从 URL 解析 JSON 的示例：
```python
import requests

response = requests.get('https://api.example.com/data')
data = response.json()

print(data)
```

这段代码从给定的 URL 获取 JSON 数据并直接将其转换为 Python 对象。

#### 使用 `jsonschema` 验证 JSON：
首先，通过 pip 安装库：

```bash
pip install jsonschema
```

然后，按照以下方式使用它：

```python
from jsonschema import validate
import jsonschema

schema = {
    "type": "object",
    "properties": {
        "name": {"type": "string"},
        "age": {"type": "number"},
        "isEmployee": {"type": "boolean"},
    },
    "required": ["name", "age", "isEmployee"]
}

# 假设 `data` 是从 JSON 解码获得的字典
try:
    validate(instance=data, schema=schema)
    print("Valid JSON data.")
except jsonschema.exceptions.ValidationError as err:
    print("Validation error:", err)
```

这个示例对您的 Python 字典（从解码的 JSON 数据获得）根据预定义的架构进行验证，确保数据符合预期的格式和类型。
