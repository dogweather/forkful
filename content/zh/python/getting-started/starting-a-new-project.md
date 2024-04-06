---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, gpt-4-0125-preview, translated from English
date: 2024-02-22 17:30:28.977337-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u865A\u62DF\u73AF\u5883\u662F\u4E00\u4E2A\
  \u5305\u542B\u6240\u6709\u5FC5\u8981\u6267\u884C\u6587\u4EF6\u7684\u72EC\u7ACB\u76EE\
  \u5F55\uFF0C\u7528\u4E8E\u4F7F\u7528Python\u9879\u76EE\u9700\u8981\u7684\u5305\u3002\
  \u5EFA\u8BAE\u4E3A\u6BCF\u4E2A\u9879\u76EE\u521B\u5EFA\u4E00\u4E2A\u865A\u62DF\u73AF\
  \u5883\uFF0C\u4EE5\u907F\u514D\u9879\u76EE\u4F9D\u8D56\u4E4B\u95F4\u7684\u51B2\u7A81\
  \u3002\u4F7F\u7528`venv`\u6A21\u5757\uFF0C\u5B83\u662F\u6807\u51C6Python\u5E93\u7684\
  \u4E00\u90E8\u5206\u3002"
lastmod: '2024-04-05T22:38:46.436153-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u865A\u62DF\u73AF\u5883\u662F\u4E00\u4E2A\
  \u5305\u542B\u6240\u6709\u5FC5\u8981\u6267\u884C\u6587\u4EF6\u7684\u72EC\u7ACB\u76EE\
  \u5F55\uFF0C\u7528\u4E8E\u4F7F\u7528Python\u9879\u76EE\u9700\u8981\u7684\u5305\u3002\
  \u5EFA\u8BAE\u4E3A\u6BCF\u4E2A\u9879\u76EE\u521B\u5EFA\u4E00\u4E2A\u865A\u62DF\u73AF\
  \u5883\uFF0C\u4EE5\u907F\u514D\u9879\u76EE\u4F9D\u8D56\u4E4B\u95F4\u7684\u51B2\u7A81\
  \u3002\u4F7F\u7528`venv`\u6A21\u5757\uFF0C\u5B83\u662F\u6807\u51C6Python\u5E93\u7684\
  \u4E00\u90E8\u5206\u3002"
title: "\u542F\u52A8\u65B0\u9879\u76EE"
weight: 1
---

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
