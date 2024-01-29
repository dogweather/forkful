---
title:                "Làm việc với TOML"
date:                  2024-01-28T22:11:40.953515-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với TOML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/working-with-toml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
TOML là một ngôn ngữ tuần tự hóa dữ liệu được thiết kế để dễ đọc và viết. Các lập trình viên sử dụng nó cho các tệp cấu hình, lưu trữ dữ liệu đơn giản và trao đổi dữ liệu giữa các ngôn ngữ do sự rõ ràng và thân thiện với con người.

## Làm thế nào:
Hãy phân tích một tệp cấu hình TOML trong C sử dụng thư viện "tomlc99". Đầu tiên, cài đặt thư viện. Sau đó, tạo một `config.toml`:

```toml
title = "Ví dụ TOML"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
```

Bây giờ, phân tích nó trong C:

```c
#include <stdio.h>
#include "toml.h"

int main() {
    FILE* fp;
    char errbuf[200];

    if (0 == (fp = fopen("config.toml", "r"))) {
        printf("Lỗi: không thể mở tệp cấu hình\n");
        return 1;
    }
    
    toml_table_t* conf = toml_parse_file(fp, errbuf, sizeof(errbuf));
    fclose(fp);
    if (0 == conf) {
        printf("Lỗi: %s\n", errbuf);
        return 1;
    }

    printf("Tiêu đề: %s\n", toml_raw_in(conf, "title"));

    toml_table_t* owner = toml_table_in(conf, "owner");
    printf("Tên Chủ sở hữu: %s\n", toml_raw_in(owner, "name"));

    toml_free(conf);
    return 0;
}
```
Kết quả mẫu:
```
Tiêu đề: "Ví dụ TOML"
Tên Chủ sở hữu: "Tom Preston-Werner"
```

## Đi sâu hơn
TOML, viết tắt của Tom's Obvious, Minimal Language, được tạo ra bởi Tom Preston-Werner vào năm 2013. Nó phục vụ như một phương án đơn giản hơn so với các định dạng như XML và YAML, tập trung vào việc trở nên dễ đọc và viết hơn cho con người. Mặc dù JSON là một phương án thay thế khác, TOML giữ một cấu trúc dễ dàng phân tích một cách trực quan bởi con người, đó là một trong những lí do chính cho sự chấp nhận của nó trong các tệp cấu hình.

Trong C, làm việc với TOML đòi hỏi phải chọn một thư viện phân tích cú pháp do ngôn ngữ không hỗ trợ nó một cách tự nhiên. Các thư viện như "tomlc99" tuân thủ C99 và cung cấp một API để giải mã văn bản TOML. Khi xem xét về hiệu suất, việc xử lý lỗi đúng cách và quản lý bộ nhớ là quan trọng vì C không có tính năng thu gom rác tự động.

## Xem thêm:
1. TOML Spec: [https://toml.io/en/](https://toml.io/en/)
2. tomlc99 GitHub repo: [https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. So sánh Các Định dạng Tuần tự hóa Dữ liệu: [https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)
