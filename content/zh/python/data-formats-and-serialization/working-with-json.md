---
title:                "使用JSON进行编程"
date:                  2024-02-03T19:24:07.498796-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用JSON进行编程"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
