---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:33.254677-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi CSV (Comma-Separated Values - Gi\xE1 Tr\u1ECB\
  \ \u0110\u01B0\u1EE3c Ph\xE2n T\xE1ch B\u1EDFi D\u1EA5u Ph\u1EA9y) bao g\u1ED3m\
  \ vi\u1EC7c ph\xE2n t\xEDch c\xFA ph\xE1p v\xE0 thao t\xE1c d\u1EEF li\u1EC7u \u0111\
  \u01B0\u1EE3c c\u1EA5u tr\xFAc d\u01B0\u1EDBi d\u1EA1ng\u2026"
lastmod: '2024-03-13T22:44:37.240683-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi CSV (Comma-Separated Values - Gi\xE1 Tr\u1ECB\
  \ \u0110\u01B0\u1EE3c Ph\xE2n T\xE1ch B\u1EDFi D\u1EA5u Ph\u1EA9y) bao g\u1ED3m\
  \ vi\u1EC7c ph\xE2n t\xEDch c\xFA ph\xE1p v\xE0 thao t\xE1c d\u1EEF li\u1EC7u \u0111\
  \u01B0\u1EE3c c\u1EA5u tr\xFAc d\u01B0\u1EDBi d\u1EA1ng h\xE0ng v\xE0 c\u1ED9t trong\
  \ \u0111\u1ECBnh d\u1EA1ng v\u0103n b\u1EA3n."
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
weight: 37
---

## Gì và Tại Sao?

Làm việc với CSV (Comma-Separated Values - Giá Trị Được Phân Tách Bởi Dấu Phẩy) bao gồm việc phân tích cú pháp và thao tác dữ liệu được cấu trúc dưới dạng hàng và cột trong định dạng văn bản. Các lập trình viên sử dụng tệp CSV vì chúng đơn giản, được hỗ trợ rộng rãi và dễ dàng nhập hoặc xuất từ cơ sở dữ liệu và bảng tính.

## Cách thực hiện:

1. Đọc một tệp CSV dòng theo dòng:
```Fish Shell
for line in (cat file.csv)
    echo $line
end
```

2. Tách các trường và in một cột cụ thể (ví dụ, cột thứ hai):
```Fish Shell
cat file.csv | while read -l line
    set -l fields (string split "," $line)
    echo $fields[2]
end
```

3. Ghi vào tệp CSV:
```Fish Shell
echo "name,age,city" > users.csv
echo "Alice,30,New York" >> users.csv
echo "Bob,25,Los Angeles" >> users.csv
```

Dữ liệu mẫu (nội dung của `users.csv`):
```
name,age,city
Alice,30,New York
Bob,25,Los Angeles
```

## Kỹ sâu

Việc xử lý CSV đã tồn tại từ những ngày đầu của máy tính cá nhân, phát triển như một định dạng đơn giản cho giao tiếp dữ liệu. Mặc dù cơ bản, nhưng sự thiếu một tiêu chuẩn cố định của CSV có thể dẫn đến các vấn đề phân tích cú pháp, như sự khác biệt về dấu phân cách và mã hóa văn bản. Mặc dù Fish Shell không có công cụ phân tích cú pháp CSV tích hợp, `awk`, `sed`, và `cut` thường được sử dụng cùng với nó cho các nhiệm vụ phức tạp hơn.

Cách tiếp cận của Fish Shell đối với CSV là dựa trên script và thủ công hơn, tận dụng khả năng thao tác chuỗi của nó để xử lý các trường CSV. Đối với việc xử lý dữ liệu nặng, cân nhắc các phương án thay thế như thư viện `pandas` của Python, hoặc các công cụ dòng lệnh như `csvkit`.

## Xem Thêm

- Bắt đầu với `awk`: [AWK - Bài học và Giới thiệu](https://www.grymoire.com/Unix/Awk.html)
- Giới thiệu về `sed`: [Sed - Giới thiệu và Bài học](https://www.grymoire.com/Unix/Sed.html)
- Tài liệu chính thức của Fish Shell: [Tài liệu Fish Shell](https://fishshell.com/docs/current/index.html)
- Tài liệu `csvkit`: [csvkit - Bộ công cụ chuyển đổi và làm việc với CSV](https://csvkit.readthedocs.io/en/latest/)
