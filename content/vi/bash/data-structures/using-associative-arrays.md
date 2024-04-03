---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:23.190674-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EA7u ti\xEAn, khai b\xE1o m\u1ED9t\
  \ m\u1EA3ng li\xEAn k\u1EBFt trong Bash."
lastmod: '2024-03-13T22:44:36.867046-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EA7u ti\xEAn, khai b\xE1o m\u1ED9t m\u1EA3ng li\xEAn k\u1EBFt trong\
  \ Bash."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

## Làm thế nào:
Đầu tiên, khai báo một mảng liên kết trong Bash:

```Bash
declare -A my_array
```

Sau đó, bạn có thể bắt đầu điền giá trị vào nó, sử dụng chuỗi làm khóa:

```Bash
my_array["name"]="Linux Journal"
my_array["topic"]="Lập trình"
```

Để truy cập một phần tử, sử dụng khóa của nó:

```Bash
echo ${my_array["name"]}  # Xuất ra: Linux Journal
```

Lặp qua khóa và giá trị cũng rất dễ dàng:

```Bash
for key in "${!my_array[@]}"; do
    echo "$key: ${my_array[$key]}"
done
```

Kết quả mẫu có thể trông như thế này:

```
name: Linux Journal
topic: Lập trình
```

Để thêm hoặc sửa đổi phần tử, chỉ cần gán một giá trị cho một khóa, tương tự như việc điền ban đầu:

```Bash
my_array["readers"]="Bạn"
```

Và để xóa một phần tử, sử dụng `unset`:

```Bash
unset my_array["topic"]
```

## Sâu hơn
Mảng liên kết được giới thiệu trong phiên bản Bash 4.0, làm cho chúng là một bổ sung tương đối mới đối với ngôn ngữ. Trước khi chúng được giới thiệu, việc xử lý mảng không chỉ mục số nguyên là khó khăn, thường yêu cầu những giải pháp tạm thời hoặc các công cụ bên ngoài như `awk` hoặc `sed`.

Về bản chất, Bash thực thi mảng liên kết sử dụng bảng băm. Cài đặt này cho phép tìm kiếm khóa một cách hiệu quả, vẫn khá ổn định bất kể kích thước mảng, một tính năng quan trọng cho hiệu suất trong thực thi kịch bản.

Mặc dù mảng liên kết trong Bash đem lại rất nhiều quyền lực và linh hoạt cho lập trình bảng lệnh, chúng đi kèm với bộ hạn chế riêng, như việc hơi vụng về khi làm việc so với mảng trong các ngôn ngữ cấp cao hơn như Python hoặc JavaScript. Đối với các nhiệm vụ xử lý dữ liệu phức tạp, có thể vẫn đáng xem xét sử dụng các công cụ hoặc ngôn ngữ khác phù hợp hơn cho công việc.

Tuy nhiên, cho nhiều nhiệm vụ kịch bản điển hình, mảng liên kết cung cấp một công cụ quý giá trong bộ công cụ của lập trình viên Bash, cho phép tạo ra kịch bản dễ đọc và dễ bảo trì hơn bằng cách cho phép sử dụng khóa chuỗi ý nghĩa thay vì chỉ mục số.
