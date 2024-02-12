---
title:                "Làm việc với CSV"
date:                  2024-01-28T22:10:12.503092-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm việc với CSV với Bash

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
