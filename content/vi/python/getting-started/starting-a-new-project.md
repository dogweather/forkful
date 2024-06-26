---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, gpt-4-0125-preview, translated from English
date: 2024-02-22 17:30:06.753054-07:00
description: "L\xE0m th\u1EBF n\xE0o: M\xF4i tr\u01B0\u1EDDng \u1EA3o l\xE0 m\u1ED9\
  t th\u01B0 m\u1EE5c t\u1EF1 ch\u1EE9a t\u1EA5t c\u1EA3 c\xE1c t\u1EC7p th\u1EF1\
  c thi c\u1EA7n thi\u1EBFt \u0111\u1EC3 s\u1EED d\u1EE5ng c\xE1c g\xF3i m\xE0 m\u1ED9\
  t d\u1EF1 \xE1n Python c\u1EA7n. N\xEAn t\u1EA1o m\xF4i tr\u01B0\u1EDDng \u1EA3\
  o cho\u2026"
lastmod: '2024-03-13T22:44:36.097827-06:00'
model: gpt-4-0125-preview
summary: "M\xF4i tr\u01B0\u1EDDng \u1EA3o l\xE0 m\u1ED9t th\u01B0 m\u1EE5c t\u1EF1\
  \ ch\u1EE9a t\u1EA5t c\u1EA3 c\xE1c t\u1EC7p th\u1EF1c thi c\u1EA7n thi\u1EBFt \u0111\
  \u1EC3 s\u1EED d\u1EE5ng c\xE1c g\xF3i m\xE0 m\u1ED9t d\u1EF1 \xE1n Python c\u1EA7\
  n."
title: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
weight: 1
---

## Làm thế nào:


### Tạo một Môi trường Ảo
Môi trường ảo là một thư mục tự chứa tất cả các tệp thực thi cần thiết để sử dụng các gói mà một dự án Python cần. Nên tạo môi trường ảo cho từng dự án để tránh xung đột giữa các phụ thuộc của dự án. Sử dụng mô-đun `venv`, là một phần của thư viện chuẩn Python.

```shell
# Thay thế 'myproject' bằng tên dự án của bạn
python3 -m venv myproject-env
```

Để kích hoạt môi trường ảo:

Trên Windows:
```shell
myproject-env\Scripts\activate.bat
```

Trên Unix hoặc MacOS:
```shell
source myproject-env/bin/activate
```

Kết quả mẫu (kết quả có thể thay đổi nhẹ tùy theo hệ điều hành):
```shell
(myproject-env) $
```

### Cài đặt Gói
Sử dụng `pip`, trình cài đặt gói cho Python, để cài đặt, nâng cấp và gỡ bỏ các gói. Dưới đây là cách bạn có thể cài đặt một thư viện bên thứ ba phổ biến, `requests`, để thực hiện các yêu cầu HTTP:

```shell
pip install requests
```

Kết quả mẫu:
```shell
Collecting requests
  Downloading requests-2.25.1-py2.py3-none-any.whl (61 kB)
     |████████████████████████████████| 61 kB 1.3 MB/s
Installing collected packages: requests
Successfully installed requests-2.25.1
```

### Thiết lập Cấu trúc Dự án
Một dự án Python điển hình có thể trông giống như sau:

```
myproject/
│
├── myproject-env/    # Môi trường ảo
├── docs/             # Tài liệu
├── tests/            # Kiểm thử đơn vị và tích hợp
│   └── __init__.py
├── myproject/        # Mã nguồn dự án
│   ├── __init__.py
│   └── main.py
├── setup.py          # Tệp thiết lập dự án
└── README.md         # Tổng quan dự án
```

### Tạo Chương trình Đầu tiên của Bạn
Tạo một tệp `main.py` bên trong thư mục `myproject`. Dưới đây là một ví dụ về một chương trình đơn giản:

```python
# myproject/myproject/main.py
def greet(name):
    return f"Xin chào, {name}!"

if __name__ == "__main__":
    print(greet("Thế giới"))
```

Chạy chương trình của bạn:

```shell
python myproject/main.py
```

Kết quả mẫu:
```shell
Xin chào, Thế giới!
```

### Sử dụng một Framework cho các Dự án Lớn hơn
Đối với các dự án lớn hơn, đặc biệt là các ứng dụng web, các framework như Django hoặc Flask là vô giá. Dưới đây là cách cài đặt Flask và tạo một ứng dụng web "Xin chào, Thế giới" đơn giản:

```shell
pip install Flask
```

Tạo một tệp `app.py` với nội dung sau:

```python
# app.py
from flask import Flask
app = Flask(__name__)

@app.route("/")
def hello_world():
    return "<p>Xin chào, Thế giới!</p>"

if __name__ == "__main__":
    app.run(debug=True)
```

Chạy ứng dụng Flask:

```shell
flask run
```

Kết quả mẫu:
```shell
 * Running on http://127.0.0.1:5000/ (Nhấn CTRL+C để thoát)
```

Truy cập vào `http://127.0.0.1:5000/` trên trình duyệt web của bạn, và bạn sẽ thấy thông điệp "Xin chào, Thế giới!".
