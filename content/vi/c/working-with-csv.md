---
title:                "Làm việc với CSV"
date:                  2024-01-28T22:10:03.160965-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm gì & Tại sao?

Làm việc với CSV (Comma-Separated Values - Giá trị tách biệt bằng dấu phẩy) có nghĩa là xử lý dữ liệu được cấu trúc dưới dạng văn bản thuần túy, nơi mỗi dòng có các trường được chia tách bằng dấu phẩy. Các lập trình viên sử dụng CSV bởi vì nó đơn giản, được hỗ trợ rộng rãi và dễ dàng tích hợp với bảng tính và cơ sở dữ liệu.

## Làm thế nào:

Hãy phân tích một tệp CSV với mã C cơ bản. Chúng ta sẽ đọc một tệp, chia mỗi dòng thành các trường và in chúng ra.

```C
#include <stdio.h>
#include <string.h>

int main() {
    FILE *fp = fopen("data.csv", "r");
    if (!fp) {
        printf("Không thể mở tệp\n");
        return 1;
    }

    char line[256];
    while (fgets(line, sizeof(line), fp)) {
        char *token = strtok(line, ",");
        while (token) {
            printf("%s\n", token);
            token = strtok(NULL, ",");
        }
    }

    fclose(fp);
    return 0;
}
```

Mẫu `data.csv`:
```
name,age,city
Alice,30,New York
Bob,25,Los Angeles
```

Mẫu output:
```
name
age
city
Alice
30
New York
Bob
25
Los Angeles
```

## Sâu hơn

Các tệp CSV đã được sử dụng từ những ngày đầu của việc tính toán cá nhân do sự đơn giản của chúng. Các lựa chọn thay thế như JSON hay XML mang lại nhiều phức tạp hơn nhưng cung cấp đại diện cho dữ liệu có cấu trúc. Khi triển khai việc phân tích CSV, cần phải lưu ý xử lý các trường hợp ngoại lệ như các trường chứa dấu phẩy hoặc xuống dòng mới, những cái này nên được đặt trong dấu ngoặc kép theo chuẩn CSV (RFC 4180).

## Xem thêm

- [RFC 4180](https://tools.ietf.org/html/rfc4180): Định dạng Chung và Kiểu MIME cho Các Tệp Giá trị Tách Biệt bởi Dấu Phẩy (CSV).
- [libcsv](http://sourceforge.net/projects/libcsv/): Một thư viện C cho việc phân tích CSV.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/csv?tab=Votes): Các cuộc thảo luận và Q/A của cộng đồng về vấn đề lập trình liên quan đến CSV.
