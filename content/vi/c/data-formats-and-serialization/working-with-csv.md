---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:11.034955-07:00
description: "Trong l\u0129nh v\u1EF1c l\u1EADp tr\xECnh, vi\u1EC7c l\xE0m vi\u1EC7\
  c v\u1EDBi c\xE1c t\u1EC7p CSV (Comma-Separated Values - Gi\xE1 tr\u1ECB \u0111\u01B0\
  \u1EE3c t\xE1ch bi\u1EC7t b\u1EB1ng d\u1EA5u ph\u1EA9y) bao g\u1ED3m vi\u1EC7c \u0111\
  \u1ECDc t\u1EEB v\xE0 vi\u1EBFt d\u1EEF\u2026"
lastmod: '2024-03-13T22:44:37.297827-06:00'
model: gpt-4-0125-preview
summary: "Trong l\u0129nh v\u1EF1c l\u1EADp tr\xECnh, vi\u1EC7c l\xE0m vi\u1EC7c v\u1EDB\
  i c\xE1c t\u1EC7p CSV (Comma-Separated Values - Gi\xE1 tr\u1ECB \u0111\u01B0\u1EE3\
  c t\xE1ch bi\u1EC7t b\u1EB1ng d\u1EA5u ph\u1EA9y) bao g\u1ED3m vi\u1EC7c \u0111\u1ECD\
  c t\u1EEB v\xE0 vi\u1EBFt d\u1EEF li\u1EC7u v\xE0o c\xE1c t\u1EC7p v\u0103n b\u1EA3\
  n \u0111\u01B0\u1EE3c t\u1ED5 ch\u1EE9c theo h\xE0ng, n\u01A1i m\u1ED7i h\xE0ng\
  \ \u0111\u1EA1i di\u1EC7n cho m\u1ED9t b\u1EA3n ghi v\xE0 c\xE1c tr\u01B0\u1EDD\
  ng c\u1EE7a m\u1ED7i b\u1EA3n ghi \u0111\u01B0\u1EE3c t\xE1ch bi\u1EC7t b\u1EB1\
  ng d\u1EA5u ph\u1EA9y."
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
weight: 37
---

## Cái gì & Tại sao?

Trong lĩnh vực lập trình, việc làm việc với các tệp CSV (Comma-Separated Values - Giá trị được tách biệt bằng dấu phẩy) bao gồm việc đọc từ và viết dữ liệu vào các tệp văn bản được tổ chức theo hàng, nơi mỗi hàng đại diện cho một bản ghi và các trường của mỗi bản ghi được tách biệt bằng dấu phẩy. Các lập trình viên thao tác với tệp CSV để dễ dàng nhập/xuất dữ liệu qua lại các hệ thống khác nhau, do sự hỗ trợ rộng rãi và đơn giản của chúng trong việc lưu trữ dữ liệu bảng.

## Làm thế nào:

### Đọc Tệp CSV
Để đọc một tệp CSV trong C, chúng ta sử dụng các hàm I/O tệp tiêu chuẩn cùng với các hàm xử lý chuỗi để phân tích từng dòng. Dưới đây là một ví dụ cơ bản về việc đọc một tệp CSV và in các trường của mỗi hàng ra bảng điều khiển.

```c
#include <stdio.h>
#include <string.h>

int main() {
    FILE *fp = fopen("data.csv", "r");
    if (!fp) {
        printf("Không thể mở tệp\n");
        return 1;
    }

    char buf[1024];
    while (fgets(buf, 1024, fp)) {
        char *field = strtok(buf, ",");
        while(field) {
            printf("%s\n", field);
            field = strtok(NULL, ",");
        }
    }

    fclose(fp);
    return 0;
}
```
Mẫu `data.csv`:
```
Tên,Tuổi,Nghề nghiệp
John Doe,29,Kỹ sư phần mềm
```

Mẫu Đầu ra:
```
Tên
Tuổi
Nghề nghiệp
John Doe
29
Kỹ sư phần mềm
```

### Viết vào Tệp CSV
Tương tự, việc viết vào một tệp CSV bao gồm việc sử dụng `fprintf` để lưu dữ liệu theo định dạng tách biệt bằng dấu phẩy.

```c
#include <stdio.h>

int main() {
    FILE *fp = fopen("output.csv", "w");
    if (!fp) {
        printf("Không thể mở tệp\n");
        return 1;
    }

    char *headers[] = {"Tên", "Tuổi", "Nghề nghiệp", NULL};
    for (int i = 0; headers[i] != NULL; i++) {
        fprintf(fp, "%s%s", headers[i], (headers[i+1] != NULL) ? "," : "\n");
    }
    fprintf(fp, "%s,%d,%s\n", "Jane Doe", 27, "Nhà khoa học dữ liệu");

    fclose(fp);
    return 0;
}
```

Nội dung Mẫu `output.csv`:
```
Tên,Tuổi,Nghề nghiệp
Jane Doe,27,Nhà khoa học dữ liệu
```

## Sâu hơn

Định dạng CSV, mặc dù có vẻ đơn giản, nhưng đi kèm với những sắc thái của nó, chẳng hạn như xử lý dấu phẩy trong các trường và bao bọc các trường bằng dấu ngoặc kép. Các ví dụ sơ bộ được hiển thị không tính đến những phức tạp như vậy, cũng không xử lý các lỗi tiềm năng một cách mạnh mẽ.

Về lịch sử, việc xử lý CSV trong C chủ yếu là thủ công do bản chất thấp của ngôn ngữ và thiếu các trừu tượng cao cấp tích hợp sẵn cho những nhiệm vụ như vậy. Việc quản lý thủ công này bao gồm mở tệp, đọc dòng, phân chia chuỗi và chuyển đổi các loại dữ liệu theo yêu cầu.

Mặc dù việc thao tác trực tiếp với tệp CSV trong C cung cấp những trải nghiệm học hỏi quý báu về I/O tệp và xử lý chuỗi, một số lựa chọn hiện đại hứa hẹn hiệu quả và quá trình ít lỗi hơn. Các thư viện như `libcsv` và `csv-parser` cung cấp các chức năng toàn diện cho việc đọc và viết tệp CSV, bao gồm hỗ trợ cho các trường được trích dẫn và dấu phân cách tùy chỉnh.

Ngoài ra, khi làm việc trong các hệ sinh thái hỗ trợ, tích hợp với các ngôn ngữ hoặc nền tảng cung cấp các hàm thao tác CSV cấp cao (như Python với thư viện `pandas` của nó) có thể là một con đường hiệu quả hơn cho các ứng dụng yêu cầu xử lý CSV nặng nề. Cách tiếp cận chéo ngôn ngữ này tận dụng khả năng hiệu suất và lập trình hệ thống của C trong khi sử dụng sự dễ dàng từ các ngôn ngữ khác cho các nhiệm vụ cụ thể như xử lý CSV.
