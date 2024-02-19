---
aliases:
- /vi/bash/checking-if-a-directory-exists/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:05.294820-07:00
description: "Vi\u1EC7c ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3\
  n t\u1EA1i kh\xF4ng l\xE0 \u0111\u1EC3 \u0111\u1EA3m b\u1EA3o r\u1EB1ng m\u1ED9\
  t th\u01B0 m\u1EE5c \u0111\xE3 c\xF3 s\u1EB5n tr\u01B0\u1EDBc khi b\u1EA1n th\u1EED\
  \ th\u1EF1c hi\u1EC7n m\u1ED9t s\u1ED1 h\xE0nh \u0111\u1ED9ng v\u1EDBi n\xF3, nh\u01B0\
  \ \u0111\u1ECDc c\xE1c\u2026"
lastmod: 2024-02-18 23:08:50.901289
model: gpt-4-0125-preview
summary: "Vi\u1EC7c ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1\
  i kh\xF4ng l\xE0 \u0111\u1EC3 \u0111\u1EA3m b\u1EA3o r\u1EB1ng m\u1ED9t th\u01B0\
  \ m\u1EE5c \u0111\xE3 c\xF3 s\u1EB5n tr\u01B0\u1EDBc khi b\u1EA1n th\u1EED th\u1EF1\
  c hi\u1EC7n m\u1ED9t s\u1ED1 h\xE0nh \u0111\u1ED9ng v\u1EDBi n\xF3, nh\u01B0 \u0111\
  \u1ECDc c\xE1c\u2026"
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc kiểm tra xem một thư mục có tồn tại không là để đảm bảo rằng một thư mục đã có sẵn trước khi bạn thử thực hiện một số hành động với nó, như đọc các tệp tin hoặc lưu các tệp mới. Các lập trình viên thực hiện việc này để tránh lỗi, duy trì dòng chảy của các script một cách mượt mà và xử lý các tình huống khi mọi thứ không nằm ở đúng vị trí mà chúng được mong đợi.

## Làm thế nào:

Dưới đây là cách bạn kiểm tra xem một thư mục có tồn tại trong Bash hay không:

```Bash
if [ -d "/path/to/dir" ]; then
  echo "Thư mục tồn tại."
else
  echo "Thư mục không tồn tại."
fi
```

Đầu ra mẫu nếu thư mục tồn tại:

```
Thư mục tồn tại.
```

Và nếu nó không tồn tại:

```
Thư mục không tồn tại.
```

Vâng, nó đơn giản như vậy. Nhưng nhớ thay thế `/path/to/dir` bằng đường dẫn thực tế mà bạn đang kiểm tra.

## Tìm hiểu sâu:

Từ xa xưa, mọi người hầu như đã làm cùng một việc, sử dụng các lệnh kiểm tra từ dòng lệnh tương tự như những gì chúng ta làm ngày nay. Bash luôn có một cách tích hợp sẵn để kiểm tra các tệp và thư mục bởi vì đó là nhu cầu cơ bản.

Bây giờ, tại sao lại là `-d` chứ không phải là thứ khác? Trong Bash, `-d` cụ thể dùng để kiểm tra sự hiện diện của một thư mục. Có các kiểm tra khác nữa, như `-f` cho các tệp hoặc `-e` cho sự tồn tại (các tệp hoặc thư mục).

Đôi khi bạn có thể thấy:

```Bash
if [[ -d "/path/to/dir" ]]; then
  # Ngoặc đôi cho một cách tiếp cận hiện đại hơn, mạnh mẽ hơn.
fi
```

Hoặc thậm chí là `&&` và `||` cho người thích viết tắt:

```Bash
[ -d "/path/to/dir" ] && echo "Thư mục tồn tại." || echo "Thư mục không tồn tại."
```

Tuy nhiên, hãy cẩn thận—phương pháp cuối cùng này có thể làm bạn hiểu lầm nếu `echo "Thư mục tồn tại."` thất bại vì một lý do nào đó, sau đó `echo "Thư mục không tồn tại."` sẽ được thực hiện ngay cả khi thư mục tồn tại. Sử dụng nó một cách thận trọng và hiểu biết.

## Xem thêm

- **Biểu thức điều kiện trong Bash**: [Hướng dẫn GNU Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- **Hướng dẫn viết kịch bản Bash**: [Hướng dẫn của Ryan](https://ryanstutorials.net/bash-scripting-tutorial/)
- **Hướng dẫn viết kịch bản Bash nâng cao**: [Dự án Tài liệu Linux](https://tldp.org/LDP/abs/html/)
