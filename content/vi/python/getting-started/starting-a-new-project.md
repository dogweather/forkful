---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:05.698888-07:00
description: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi \u0111\u1EC1\
  u b\u1EAFt \u0111\u1EA7u t\u1EEB vi\u1EC7c t\u1EA1o m\u1ED9t th\u01B0 m\u1EE5c m\u1EDB\
  i v\u1EDBi c\xE1c t\u1EC7p \u0111\u01B0\u1EE3c thi\u1EBFt l\u1EADp s\u1EB5n cho\
  \ cu\u1ED9c phi\xEAu l\u01B0u code m\u1EDBi c\u1EE7a b\u1EA1n. \u0110\xF3 gi\u1ED1\
  ng nh\u01B0 vi\u1EC7c kh\u1EDFi\u2026"
lastmod: 2024-02-19 22:04:55.291736
model: gpt-4-0125-preview
summary: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi \u0111\u1EC1u b\u1EAF\
  t \u0111\u1EA7u t\u1EEB vi\u1EC7c t\u1EA1o m\u1ED9t th\u01B0 m\u1EE5c m\u1EDBi v\u1EDB\
  i c\xE1c t\u1EC7p \u0111\u01B0\u1EE3c thi\u1EBFt l\u1EADp s\u1EB5n cho cu\u1ED9\
  c phi\xEAu l\u01B0u code m\u1EDBi c\u1EE7a b\u1EA1n. \u0110\xF3 gi\u1ED1ng nh\u01B0\
  \ vi\u1EC7c kh\u1EDFi\u2026"
title: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
---

{{< edit_this_page >}}

## Gì và Tại sao?

Bắt đầu một dự án mới đều bắt đầu từ việc tạo một thư mục mới với các tệp được thiết lập sẵn cho cuộc phiêu lưu code mới của bạn. Đó giống như việc khởi công xây dựng tại một công trường xây dựng nhưng đối với các lập trình viên. Chúng ta làm điều này để biến ý tưởng thành phần mềm hoạt động, tổ chức code của mình và quản lý độ phức tạp ngay từ đầu.

## Làm thế nào:

Hãy bắt đầu một dự án Python. Đầu tiên, tạo một thư mục mới:

```bash
mkdir my_new_project
cd my_new_project
```

Bây giờ, thiết lập một môi trường ảo - điều này giữ cho các phụ thuộc dự án của chúng ta gọn gàng và ngăn nắp:

```bash
python -m venv venv
source venv/bin/activate # Trên Windows, sử dụng `venv\Scripts\activate`
```

Với mảnh đất ảo đã được chuẩn bị, gieo hạt giống cho dự án của bạn với tệp `main.py`:

```bash
touch main.py
echo "print('Xin chào, dự án mới!')" > main.py
python main.py
```

Đầu ra:
```plaintext
Xin chào, dự án mới!
```

Làm một cách cẩn thận, hãy gắn kết các phụ thuộc sớm. Ngay cả khi chưa tồn tại bất kỳ cái nào:

```bash
pip freeze > requirements.txt
```

Và đó là hình mẫu ban đầu của dự án của bạn. Từ đây, nó sẽ phát triển.

## Tìm hiểu sâu hơn

Trong quá khứ, nhiều lập trình viên chỉ việc tùy tiện bắt đầu code trong một tệp đơn lẻ. Hỗn loạn thường xuyên xảy ra khi dự án phát triển. Ngày nay, chúng ta đã có những thực hành tốt hơn.

Đối với Python, chúng ta có các quy ước như PEP 8 cho hướng dẫn về phong cách. Cũng có những công cụ như `cookiecutter` tạo dự án từ các mẫu. Muốn một ứng dụng web? Đã có mẫu cho điều đó. Nó được thiết lập để tiết kiệm thời gian cho bạn.

Mặt khác, bạn có thể muốn tự làm theo cách mà chúng tôi đã trình bày ở trên. Phương pháp này cho bạn tổng kiểm soát, xây dựng dự án của bạn từ đầu. Chỉ cần nhớ theo dõi các phụ thuộc với `requirements.txt`. Điều này rất quan trọng khi bạn chia sẻ dự án hoặc triển khai nó.

## Xem thêm

- [The Hitchhiker's Guide to Python](https://docs.python-guide.org/) - Hướng dẫn mang tính chất phê bình về các thực hành tốt nhất trong Python.
- [PEP 8 -- Style Guide for Python Code](https://peps.python.org/pep-0008/) - Kinh thánh về phong cách cho các nhà phát triển Python.
- [Cookiecutter](https://github.com/cookiecutter/cookiecutter) - Một tiện ích dòng lệnh để tạo dự án từ các mẫu.
