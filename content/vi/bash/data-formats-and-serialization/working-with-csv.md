---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:12.503092-07:00
description: "C\xE1i g\xEC & T\u1EA1i sao? L\xE0m vi\u1EC7c v\u1EDBi CSV, vi\u1EBF\
  t t\u1EAFt c\u1EE7a \"Comma-Separated Values\" (Gi\xE1 tr\u1ECB t\xE1ch b\u1EB1\
  ng d\u1EA5u ph\u1EA9y), bao g\u1ED3m qu\xE1 tr\xECnh ph\xE2n t\xEDch v\xE0 thao\
  \ t\xE1c d\u1EEF li\u1EC7u\u2026"
lastmod: '2024-03-13T22:44:36.907008-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi CSV, vi\u1EBFt t\u1EAFt c\u1EE7a \"Comma-Separated\
  \ Values\" (Gi\xE1 tr\u1ECB t\xE1ch b\u1EB1ng d\u1EA5u ph\u1EA9y), bao g\u1ED3m\
  \ qu\xE1 tr\xECnh ph\xE2n t\xEDch v\xE0 thao t\xE1c d\u1EEF li\u1EC7u trong \u0111\
  \u1ECBnh d\u1EA1ng v\u0103n b\u1EA3n b\u1EA3ng."
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
weight: 37
---

## Cái gì & Tại sao?
Làm việc với CSV, viết tắt của "Comma-Separated Values" (Giá trị tách bằng dấu phẩy), bao gồm quá trình phân tích và thao tác dữ liệu trong định dạng văn bản bảng. Các lập trình viên làm điều này bởi vì CSV là một định dạng tập tin phổ biến, đơn giản, được sử dụng để trao đổi dữ liệu giữa các ứng dụng và hệ thống khác nhau.

## Làm thế nào:


### Đọc từ tập tin CSV:
```Bash
while IFS=, read -r col1 col2 col3
do
  echo "Cột 1: $col1 | Cột 2: $col2 | Cột 3: $col3"
done < myfile.csv
```

Kết quả mẫu:

```
Cột 1: data1 | Cột 2: data2 | Cột 3: data3
```

### Ghi vào tập tin CSV:
```Bash
echo "data1,data2,data3" > myfile.csv
```

### Chèn thêm vào tập tin CSV:
```Bash
echo "data4,data5,data6" >> myfile.csv
```

## Đào sâu
Định dạng CSV có nguồn gốc từ thời kỳ đầu của máy tính và đã trở thành trụ cột trong trao đổi dữ liệu vì nó được hỗ trợ bởi nhiều loại phần mềm đa dạng. Mặc dù Bash có thể xử lý các tập tin CSV, nhưng nó không được trang bị sẵn cho việc phân tích phức tạp. Các lựa chọn thay thế cho các nhiệm vụ phức tạp hơn bao gồm AWK, Sed, hoặc sử dụng một ngôn ngữ lập trình đầy đủ như Python. Các chi tiết thực hiện cần xem xét khi làm việc với CSV trong Bash bao gồm xử lý các ký tự đặc biệt, trích dẫn phức tạp và ngắt dòng trong các trường.

## Xem thêm
- [Tài liệu GNU Coreutils](https://www.gnu.org/software/coreutils/)
- [Sổ tay tham khảo Bash](https://www.gnu.org/software/bash/manual/)
- [Giới thiệu về AWK](https://www.gnu.org/software/gawk/manual/gawk.html)
- [Sed qua Ví dụ](https://www.gnu.org/software/sed/manual/sed.html)

Đối với các thao tác CSV nâng cao hơn:
- [Tài liệu Mô-đun CSV của Python](https://docs.python.org/3/library/csv.html)
- [Thư viện Pandas cho Python](https://pandas.pydata.org/)
