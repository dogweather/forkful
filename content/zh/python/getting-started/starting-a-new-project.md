---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, gpt-4-0125-preview, translated from English
date: 2024-02-22 17:30:28.977337-07:00
description: "\u5728Python\u4E2D\u542F\u52A8\u4E00\u4E2A\u65B0\u9879\u76EE\u5C31\u662F\
  \u4ECE\u4E00\u5F00\u59CB\u5C31\u5EFA\u7ACB\u4E00\u4E2A\u7ED3\u6784\u5316\u3001\u53EF\
  \u7EF4\u62A4\u7684\u6846\u67B6\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u786E\u4FDD\u4ED6\u4EEC\u7684\u4EE3\u7801\u6613\u4E8E\u9605\u8BFB\u3001\u8C03\
  \u8BD5\u548C\u534F\u4F5C\uFF0C\u5C24\u5176\u662F\u968F\u7740\u9879\u76EE\u548C\u56E2\
  \u961F\u7684\u6210\u957F\u3002"
lastmod: '2024-03-11T00:14:21.022552-06:00'
model: gpt-4-0125-preview
summary: "\u5728Python\u4E2D\u542F\u52A8\u4E00\u4E2A\u65B0\u9879\u76EE\u5C31\u662F\
  \u4ECE\u4E00\u5F00\u59CB\u5C31\u5EFA\u7ACB\u4E00\u4E2A\u7ED3\u6784\u5316\u3001\u53EF\
  \u7EF4\u62A4\u7684\u6846\u67B6\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u786E\u4FDD\u4ED6\u4EEC\u7684\u4EE3\u7801\u6613\u4E8E\u9605\u8BFB\u3001\u8C03\
  \u8BD5\u548C\u534F\u4F5C\uFF0C\u5C24\u5176\u662F\u968F\u7740\u9879\u76EE\u548C\u56E2\
  \u961F\u7684\u6210\u957F\u3002"
title: "\u542F\u52A8\u65B0\u9879\u76EE"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在Python中启动一个新项目就是从一开始就建立一个结构化、可维护的框架。程序员这样做是为了确保他们的代码易于阅读、调试和协作，尤其是随着项目和团队的成长。

## 如何操作：

### 创建虚拟环境
虚拟环境是一个包含所有必要执行文件的独立目录，用于使用Python项目需要的包。建议为每个项目创建一个虚拟环境，以避免项目依赖之间的冲突。使用`venv`模块，它是标准Python库的一部分。

```shell
# 用你的项目名称替换'myproject'
python3 -m venv myproject-env
```

激活虚拟环境：

在Windows上：
```shell
myproject-env\Scripts\activate.bat
```

在Unix或MacOS上：
```shell
source myproject-env/bin/activate
```

示例输出（输出可能因操作系统略有不同）：
```shell
(myproject-env) $
```

### 安装包
用`pip`，即Python的包安装工具，来安装、升级和移除包。以下是如何安装一个流行的第三方库`requests`以发起HTTP请求：

```shell
pip install requests
```

示例输出：
```shell
收集requests
  下载requests-2.25.1-py2.py3-none-any.whl (61 kB)
     |████████████████████████████████| 61 kB 1.3 MB/s 
安装收集的包：requests
成功安装requests-2.25.1
```

### 建立项目结构
一个典型的Python项目可能看起来像这样：

```
myproject/
│
├── myproject-env/    # 虚拟环境
├── docs/             # 文档
├── tests/            # 单元和集成测试
│   └── __init__.py
├── myproject/        # 项目源代码
│   ├── __init__.py
│   └── main.py
├── setup.py          # 项目设置文件
└── README.md         # 项目概览
```

### 创建你的第一个程序
在`myproject`目录内创建一个名为`main.py`的文件。这是一个简单程序的示例：

```python
# myproject/myproject/main.py
def greet(name):
    return f"Hello, {name}!"

if __name__ == "__main__":
    print(greet("World"))
```

运行你的程序：

```shell
python myproject/main.py
```

示例输出：
```shell
Hello, World!
```

### 对于更大的项目使用框架
对于更大的项目，尤其是网络应用，像Django或Flask这样的框架至关重要。以下是如何安装Flask并创建一个简单的"Hello, World"网络应用：

```shell
pip install Flask
```

创建一个名为`app.py`且内容如下的文件：

```python
# app.py
from flask import Flask
app = Flask(__name__)

@app.route("/")
def hello_world():
    return "<p>Hello, World!</p>"

if __name__ == "__main__":
    app.run(debug=True)
```

运行Flask应用：

```shell
flask run
```

示例输出：
```shell
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
```

在你的网络浏览器导航到`http://127.0.0.1:5000/`，你应该会看到"Hello, World!"消息。
