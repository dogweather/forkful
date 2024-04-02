---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:45.320918-07:00
description: "TOML (Tom's Obvious, Minimal Language) l\xE0 m\u1ED9t \u0111\u1ECBnh\
  \ d\u1EA1ng file c\u1EA5u h\xECnh d\u1EC5 \u0111\u1ECDc do ng\u1EEF ngh\u0129a r\xF5\
  \ r\xE0ng c\u1EE7a n\xF3. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3\
  \ cho c\xE1c file c\u1EA5u\u2026"
lastmod: '2024-03-13T22:44:37.299187-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng file c\u1EA5u h\xECnh d\u1EC5 \u0111\u1ECDc do ng\u1EEF ngh\u0129a r\xF5 r\xE0\
  ng c\u1EE7a n\xF3. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 cho\
  \ c\xE1c file c\u1EA5u\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

## Cái gì & Tại sao?

TOML (Tom's Obvious, Minimal Language) là một định dạng file cấu hình dễ đọc do ngữ nghĩa rõ ràng của nó. Các lập trình viên sử dụng nó cho các file cấu hình trong ứng dụng bởi vì sự đơn giản và dễ đọc của nó làm cho nó trở thành lựa chọn xuất sắc hơn so với các định dạng như XML hoặc JSON trong một số bối cảnh.

## Làm thế nào:

Để làm việc với TOML trong C, bạn đầu tiên cần một thư viện có khả năng phân tích cú pháp các file TOML, vì thư viện chuẩn C không bao gồm chức năng này. Một lựa chọn phổ biến là `tomlc99`, một bộ phân tích cú pháp TOML nhẹ cho C99. Dưới đây là hướng dẫn nhanh để đọc một file cấu hình TOML đơn giản:

Đầu tiên, đảm bảo bạn đã cài đặt và liên kết đúng cách `tomlc99` trong dự án của mình.

**File TOML mẫu (`config.toml`):**
```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

**Mã C để phân tích file này:**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *configFile;
    configFile = fopen("config.toml", "r");
    if (!configFile) {
        perror("Cannot open file");
        return EXIT_FAILURE;
    }

    toml_table_t *config = toml_parse_file(configFile, NULL, 0);
    if (!config) {
        fprintf(stderr, "Error parsing file\n");
        fclose(configFile);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(config, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("Máy chủ cơ sở dữ liệu: %s\n", server);

        toml_array_t *ports = toml_array_in(database, "ports");
        for (int i = 0; i < toml_array_nelem(ports); i++) {
            int64_t port;
            toml_int_at(ports, i, &port);
            printf("Cổng %d: %ld\n", i, port);
        }
    }

    toml_free(config);
    fclose(configFile);
    return EXIT_SUCCESS;
}
```

**Kết quả:**
```
Máy chủ cơ sở dữ liệu: "192.168.1.1"
Cổng 0: 8001
Cổng 1: 8001
Cổng 2: 8002
```

## Tìm hiểu sâu

TOML được tạo ra bởi Tom Preston-Werner, đồng sáng lập GitHub, như một phản hồi đối với các hạn chế mà ông nhận thấy trong các định dạng file cấu hình khác. Mục tiêu của nó là sự rõ ràng và không mơ hồ, cả đối với con người và máy tính, để đọc và viết mà không cần các quy tắc phức tạp trong việc phân tích cú pháp. Trong hệ sinh thái C, TOML không phải là một công dân hạng nhất như nó có thể là trong các ngôn ngữ cấp cao hơn như Rust với `serde_toml` của nó hay Python với `toml`, mà có các thư viện với hỗ trợ bản địa. Thay vào đó, các nhà phát triển C cần phải dựa vào các thư viện bên ngoài như `tomlc99`, nhưng điều này là điển hình với sự nhấn mạnh vào sự tối giản và hiệu suất của C.

Mặc dù TOML được ca ngợi vì sự rõ ràng của nó, khi chọn một định dạng file cấu hình, điều quan trọng là phải xem xét nhu cầu của dự án. Trong các kịch bản yêu cầu cấu trúc phức tạp hơn hoặc tương tác với các API web, JSON hoặc thậm chí YAML có thể cung cấp sự phù hợp tốt hơn bất chấp sự phức tạp tăng lên của chúng. TOML tỏa sáng trong các cấu hình nơi sự dễ đọc và đơn giản là quan trọng nhất, không nhất thiết là nơi cần những cấu trúc dữ liệu tiên tiến nhất.
