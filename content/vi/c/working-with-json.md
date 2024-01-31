---
title:                "Làm việc với JSON"
date:                  2024-01-28T22:10:34.459857-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với JSON"

category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

JSON, viết tắt của JavaScript Object Notation, là một định dạng nhẹ cho việc trao đổi dữ liệu. Lập trình viên sử dụng nó bởi vì nó dễ đọc và viết đối với con người, và máy móc có thể phân tích và tạo ra, khiến nó trở thành lựa chọn hàng đầu cho các API và tệp cấu hình.

## Làm thế nào:

Trong C, bạn thường sử dụng một thư viện như cJSON hoặc Jansson để xử lý JSON. Dưới đây là cách bạn sẽ phân tích và tạo JSON với cJSON:

```C
#include <stdio.h>
#include "cJSON.h"

int main() {
    // JSON chúng tôi đang phân tích
    char text[] = "{\"name\": \"John\", \"age\": 30}";

    // Phân tích JSON
    cJSON *json = cJSON_Parse(text);
    if (json == NULL) {
        const char *error_ptr = cJSON_GetErrorPtr();
        if (error_ptr != NULL) {
            fprintf(stderr, "Lỗi trước: %s\n", error_ptr);
        }
        return 1;
    }

    // Lấy giá trị
    const cJSON *name = cJSON_GetObjectItemCaseSensitive(json, "name");
    const cJSON *age = cJSON_GetObjectItemCaseSensitive(json, "age");

    // Kiểm tra xem các item có hợp lệ và đúng loại không
    if (cJSON_IsString(name) && (name->valuestring != NULL)) {
        printf("Tên: %s\n", name->valuestring);
    }
    if (cJSON_IsNumber(age)) {
        printf("Tuổi: %d\n", age->valueint);
    }

    // Dọn dẹp
    cJSON_Delete(json);
    return 0;
}
```

Kết quả mẫu:
```
Tên: John
Tuổi: 30
```

## Sâu hơn

JSON đã ra đời từ JavaScript, nhưng tính đơn giản của nó đã khiến nó trở thành tiêu chuẩn trong nhiều ngôn ngữ. Trước JSON, XML là người chơi chính trong việc trao đổi dữ liệu nhưng thiếu đi sự tối giản mà JSON mang lại. Lua, YAML và TOML là những lựa chọn khác, mỗi người với các trường hợp sử dụng và phong cách cú pháp của riêng mình. Việc triển khai JSON trong C từ đầu đòi hỏi phải hiểu biết về token, bộ phân tích và bộ tự động hóa dữ liệu. Điều này không đơn giản, do đó có sự ưa chuộng cho các thư viện mạnh mẽ.

## Xem Thêm

- Thư viện cJSON: https://github.com/DaveGamble/cJSON
- Thư viện Jansson: https://digip.org/jansson/
- Spec JSON: https://www.json.org/json-en.html
- So sánh các định dạng tuần tự hóa dữ liệu: https://en.wikipedia.org/wiki/Comparison_of_data_serialization_formats
