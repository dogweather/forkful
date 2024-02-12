---
title:                "Bắt đầu một dự án mới"
aliases: - /vi/python/starting-a-new-project.md
date:                  2024-01-28T22:09:05.698888-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bắt đầu một dự án mới"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
