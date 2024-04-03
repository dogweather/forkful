---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:11.142020-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Fish, b\u1EA1n s\u1EED d\u1EE5ng d\u1EA5\
  u ngo\u1EB7c k\xE9p v\xE0 \u0111\u1EB7t bi\u1EBFn ho\u1EB7c l\u1EC7nh m\xE0 b\u1EA1\
  n mu\u1ED1n n\u1ED9i suy v\u1EDBi d\u1EA5u \u0111\xF4 la `$` ngay trong chu\u1ED7\
  i."
lastmod: '2024-03-13T22:44:37.189676-06:00'
model: gpt-4-0125-preview
summary: "Trong Fish, b\u1EA1n s\u1EED d\u1EE5ng d\u1EA5u ngo\u1EB7c k\xE9p v\xE0\
  \ \u0111\u1EB7t bi\u1EBFn ho\u1EB7c l\u1EC7nh m\xE0 b\u1EA1n mu\u1ED1n n\u1ED9i\
  \ suy v\u1EDBi d\u1EA5u \u0111\xF4 la `$` ngay trong chu\u1ED7i."
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

## Làm thế nào:
Trong Fish, bạn sử dụng dấu ngoặc kép và đặt biến hoặc lệnh mà bạn muốn nội suy với dấu đô la `$` ngay trong chuỗi.

```fish
set name "world"
echo "Hello, $name!"
```

Kết quả:
```
Hello, world!
```

Để bao gồm kết quả của một lệnh trong một chuỗi:

```fish
echo "I have (count (ls)) files in this directory."
```

Kết quả có thể là:
```
I have 9 files in this directory.
```

Các biến và lệnh được đánh giá và gói gọn ngay vào vị trí bạn đặt chúng.

## Sâu hơn
Trước Fish và các shell hiện đại khác, bạn thường phải sử dụng một tổ hợp cồng kềnh của dấu ngoặc và ghép chuỗi—hoặc dựa vào các công cụ bên ngoài—để đưa biến vào chuỗi.

Trong bash, ví dụ, nó sẽ trông như thế này:

```bash
name="world"
echo "Hello, "$name"!"
```

Không mượt mà lắm, phải không?

Fish không chỉ làm cho quy trình này trở nên mạch lạc hơn mà còn xử lý lỗi một cách nhẹ nhàng hơn. Nếu một biến không tồn tại, Fish sẽ chèn một chuỗi trống, giảm khả năng crash từ việc xử lý không đúng các nội suy.

Các lựa chọn thay thế cho nội suy trực tiếp bao gồm sử dụng lệnh `printf`:

```fish
set animal "narwhal"
printf "The %s is an awesome creature!" $animal
```

Kết quả:
```
The narwhal is an awesome creature!
```

Trong trường hợp này, `%s` là một ký tự đại diện cho biến chuỗi `$animal` được `printf` thay thế.

Về mặt triển khai, khi Fish xử lý dòng lệnh, nó phân tách các chuỗi được bao bởi dấu ngoặc kép và thay thế các biến bằng giá trị của chúng một cách trực tiếp. Điều này rất tinh tế và mô phỏng sự nội suy biến tìm thấy trong các ngôn ngữ cấp cao như Ruby hoặc PHP.

## Xem thêm
Để biết thêm thông tin về việc thao tác chuỗi và viết kịch bản với Fish, hãy xem:

- [Tài liệu Fish Shell: Dấu ngoặc](https://fishshell.com/docs/current/index.html#quotes)
- [Hướng dẫn Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Stack Overflow: Cách sử dụng biến trong một lệnh trong Fish](https://stackoverflow.com/questions/2763006/how-to-use-variables-in-a-command-in-fish)
