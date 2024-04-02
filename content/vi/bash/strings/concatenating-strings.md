---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:10.382100-07:00
description: "N\u1ED1i chu\u1ED7i trong Bash c\xF3 ngh\u0129a l\xE0 gh\xE9p hai ho\u1EB7\
  c nhi\u1EC1u \u0111o\u1EA1n v\u0103n b\u1EA3n l\u1EA1i v\u1EDBi nhau. L\u1EADp tr\xEC\
  nh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u \u0111\xF3 \u0111\u1EC3 x\xE2y d\u1EF1\
  ng c\xE1c l\u1EC7nh, t\u1EA1o \u0111\u01B0\u1EDDng d\u1EABn t\u1EC7p,\u2026"
lastmod: '2024-03-13T22:44:36.865810-06:00'
model: gpt-4-0125-preview
summary: "N\u1ED1i chu\u1ED7i trong Bash c\xF3 ngh\u0129a l\xE0 gh\xE9p hai ho\u1EB7\
  c nhi\u1EC1u \u0111o\u1EA1n v\u0103n b\u1EA3n l\u1EA1i v\u1EDBi nhau. L\u1EADp tr\xEC\
  nh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u \u0111\xF3 \u0111\u1EC3 x\xE2y d\u1EF1\
  ng c\xE1c l\u1EC7nh, t\u1EA1o \u0111\u01B0\u1EDDng d\u1EABn t\u1EC7p,\u2026"
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
weight: 3
---

## Cái gì & Tại sao?

Nối chuỗi trong Bash có nghĩa là ghép hai hoặc nhiều đoạn văn bản lại với nhau. Lập trình viên thực hiện điều đó để xây dựng các lệnh, tạo đường dẫn tệp, hoặc đơn giản là định dạng văn bản đầu ra.

## Cách thực hiện:

Dưới đây là cách nhanh chóng để nối các chuỗi của bạn trong Bash:

```Bash
# Nối bằng cách đặt các chuỗi cạnh nhau
greeting="Xin chào, "
name="thế giới!"
welcome=$greeting$name
echo $welcome  # Đầu ra: Xin chào, thế giới!

# Sử dụng dấu ngoặc nhọn để làm rõ
version="phiên bản"
number=1
full_version=${version}_$number
echo $full_version  # Đầu ra: phiên bản_1

# Nối với biến và ký tự
timestamp=$(date +%Y%m%d)  # Lấy ngày hiện tại theo định dạng YYYYMMDD
filename="sao_lưu_${timestamp}.tar.gz"
echo $filename  # Đầu ra: sao_lưu_20230315.tar.gz
```

## Đào sâu hơn

Trở lại những ngày trước khi giao diện người dùng đồ họa thống trị, dòng lệnh và kịch bản là vua của sự tương tác máy tính. Việc nối chuỗi luôn là thiết yếu bởi vì nó cho phép thao tác lệnh và tệp linh hoạt.

Một phương thức thay thế lịch sử là lệnh `expr`, giờ đây cảm thấy như một cổ vật:

```Bash
older_way=$(expr $greeting $name)
```

Nhưng Bash nói, "Ai cần phải vất vả như vậy?" và làm cho nó trở nên tự nhiên. Thế nào? Vâng, Bash xử lý các chuỗi như những người bạn ấm áp vậy: đặt chúng cạnh nhau và chúng sẽ ôm nhau thành một chuỗi dài.

Bên dưới lớp vỏ, Bash xử lý điều này mà không cần bất kỳ chức năng hoặc cú pháp đặc biệt nào cho việc nối. Các từ hoặc biến chỉ đơn giản chảy vào nhau. Tuy nhiên, nếu biến của bạn có thể bắt đầu với một số hoặc một dấu gạch dưới, bạn thường sẽ bọc chúng trong dấu ngoặc nhọn để tránh nhầm lẫn với tên biến khác.

Có một điều cần lưu ý: khoảng trắng quan trọng. Nếu bạn không cẩn thận, bạn có thể kết thúc với những khoảng cách không mong muốn hoặc một lộn xộn đè nén.

Một phương thức thay thế hiện tại là sử dụng hàm `printf`, cung cấp cho bạn nhiều kiểm soát hơn về định dạng:

```Bash
printf -v full_greeting "%s%s" "$greeting" "$name"
echo $full_greeting  # Đầu ra: Xin chào, thế giới!
```

## Xem thêm

- [Hướng dẫn GNU Bash](https://www.gnu.org/software/bash/manual/) cho tất cả mọi thứ về Bash.
- [Hướng dẫn Lập trình nâng cao với Bash-Scripting](https://tldp.org/LDP/abs/html/) cho các bài tập và nhiều ví dụ hơn.
