---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:07.498796-07:00
description: "\u4F7F\u7528 JSON (JavaScript \u5BF9\u8C61\u8868\u793A\u6CD5) \u6D89\
  \u53CA\u5230\u5C06 JSON \u683C\u5F0F\u7684\u5B57\u7B26\u4E32\u89E3\u6790\u6210 Python\
  \ \u5BF9\u8C61\uFF0C\u53CD\u4E4B\u4EA6\u7136\u3002\u8FD9\u5BF9\u4E8E web \u548C\
  \ API \u5F00\u53D1\u81F3\u5173\u91CD\u8981\uFF0C\u56E0\u4E3A JSON \u662F\u670D\u52A1\
  \u5668\u4E0E\u5BA2\u6237\u7AEF\u4EA4\u6362\u6570\u636E\u7684\u901A\u7528\u8BED\u8A00\
  \u3002"
lastmod: '2024-03-13T22:44:47.279883-06:00'
model: gpt-4-0125-preview
summary: "\u4F7F\u7528 JSON (JavaScript \u5BF9\u8C61\u8868\u793A\u6CD5) \u6D89\u53CA\
  \u5230\u5C06 JSON \u683C\u5F0F\u7684\u5B57\u7B26\u4E32\u89E3\u6790\u6210 Python\
  \ \u5BF9\u8C61\uFF0C\u53CD\u4E4B\u4EA6\u7136\u3002\u8FD9\u5BF9\u4E8E web \u548C\
  \ API \u5F00\u53D1\u81F3\u5173\u91CD\u8981\uFF0C\u56E0\u4E3A JSON \u662F\u670D\u52A1\
  \u5668\u4E0E\u5BA2\u6237\u7AEF\u4EA4\u6362\u6570\u636E\u7684\u901A\u7528\u8BED\u8A00\
  \u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
weight: 38
---

## 什么 & 为什么？

使用 JSON (JavaScript 对象表示法) 涉及到将 JSON 格式的字符串解析成 Python 对象，反之亦然。这对于 web 和 API 开发至关重要，因为 JSON 是服务器与客户端交换数据的通用语言。

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
