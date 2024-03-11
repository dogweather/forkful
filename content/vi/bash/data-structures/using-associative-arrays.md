---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:23.190674-07:00
description: "M\u1EA3ng li\xEAn k\u1EBFt l\xE0 nh\u01B0 nh\u1EEFng m\u1EA3ng t\u0103\
  ng c\u01B0\u1EDDng, cho ph\xE9p b\u1EA1n s\u1EED d\u1EE5ng chu\u1ED7i l\xE0m ch\u1EC9\
  \ m\u1EE5c thay v\xEC ch\u1EC9 c\xF3 s\u1ED1 nguy\xEAn. L\u1EADp tr\xECnh vi\xEA\
  n s\u1EED d\u1EE5ng ch\xFAng cho c\u1EA5u tr\xFAc d\u1EEF\u2026"
lastmod: '2024-03-11T00:14:10.155738-06:00'
model: gpt-4-0125-preview
summary: "M\u1EA3ng li\xEAn k\u1EBFt l\xE0 nh\u01B0 nh\u1EEFng m\u1EA3ng t\u0103ng\
  \ c\u01B0\u1EDDng, cho ph\xE9p b\u1EA1n s\u1EED d\u1EE5ng chu\u1ED7i l\xE0m ch\u1EC9\
  \ m\u1EE5c thay v\xEC ch\u1EC9 c\xF3 s\u1ED1 nguy\xEAn. L\u1EADp tr\xECnh vi\xEA\
  n s\u1EED d\u1EE5ng ch\xFAng cho c\u1EA5u tr\xFAc d\u1EEF\u2026"
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Mảng liên kết là như những mảng tăng cường, cho phép bạn sử dụng chuỗi làm chỉ mục thay vì chỉ có số nguyên. Lập trình viên sử dụng chúng cho cấu trúc dữ liệu phức tạp hơn, làm cho việc xử lý dữ liệu không gọn gàng vừa với một danh sách tuần tự trở nên dễ dàng hơn.

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
