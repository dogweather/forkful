---
title:                "Làm việc với JSON"
aliases:
- /vi/c/working-with-json/
date:                  2024-02-03T18:12:43.995380-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm việc với JSON trong C

### Lý do & Tại sao?

Làm việc với JSON (JavaScript Object Notation) trong C bao gồm việc phân tích cú pháp, tạo và thao tác với cấu trúc dữ liệu JSON. Lập trình viên thực hiện việc này để cho phép giao tiếp với dịch vụ web, lưu trữ dữ liệu hoặc cấu hình tệp trong một định dạng nhẹ và dễ đọc.

### Cách thức:

Để làm việc với JSON trong C, bạn thường sẽ sử dụng một thư viện như `jansson` hoặc `json-c` do C không hỗ trợ sẵn cho JSON. Ở đây, chúng tôi sẽ tập trung vào `jansson` vì tính dễ sử dụng và được bảo trì tích cực. Đầu tiên, cài đặt thư viện (ví dụ, sử dụng trình quản lý gói như `apt` trên Ubuntu: `sudo apt-get install libjansson-dev`).

Bắt đầu bằng cách phân tích một chuỗi JSON và truy cập nội dung của nó:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    const char *json_string = "{\"name\":\"John Doe\",\"age\":30}";
    json_error_t error;
    json_t *root = json_loads(json_string, 0, &error);
    
    if(!root) {
        fprintf(stderr, "error: on line %d: %s\n", error.line, error.text);
        return 1;
    }
    
    const char *name;
    int age;
    json_unpack(root, "{s:s, s:i}", "name", &name, "age", &age);
    
    printf("Tên: %s\nTuổi: %d\n", name, age);
    
    json_decref(root);
    return 0;
}
```

Kết quả mẫu:
```
Tên: John Doe
Tuổi: 30
```

Tiếp theo, tạo và xuất một đối tượng JSON:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    json_t *root = json_object();
    json_object_set_new(root, "name", json_string("Jane Doe"));
    json_object_set_new(root, "age", json_integer(25));
    
    char *json_dump = json_dumps(root, JSON_ENCODE_ANY);
    printf("%s\n", json_dump);
    
    free(json_dump);
    json_decref(root);
    return 0;
}
```

Kết quả mẫu:
```
{"name": "Jane Doe", "age": 25}
```

Những ví dụ này minh họa cơ bản việc nạp một chuỗi JSON, giải nén giá trị của nó, tạo một đối tượng JSON mới, và sau đó xuất nó dưới dạng một chuỗi.

### Tìm hiểu sâu hơn

Nhu cầu làm việc với JSON trong C xuất phát từ việc web chấp nhận JSON như một định dạng chính cho trao đổi dữ liệu. Sự đơn giản và hiệu quả của JSON đã nhanh chóng vượt qua XML, mặc dù ban đầu C không hỗ trợ trực tiếp việc thao tác JSON. Các giải pháp đầu tiên liên quan đến việc thao tác chuỗi thủ công - dễ phạm lỗi và kém hiệu quả. Các thư viện như `jansson` và `json-c` đã xuất hiện để lấp đầy khoảng trống này, cung cấp các API mạnh mẽ cho việc phân tích cú pháp, xây dựng và chuẩn hóa JSON.

Trong khi `jansson` mang lại sự đơn giản và dễ sử dụng, `json-c` có thể hấp dẫn những người tìm kiếm một bộ tính năng rộng lớn hơn. Tuy nhiên, các lựa chọn thay thế như thư viện phân tích cú pháp trong C++ cung cấp kiểu trừu tượng phức tạp hơn, nhờ sự hỗ trợ của cấu trúc dữ liệu phức tạp và thư viện tiêu chuẩn của ngôn ngữ đó. Tuy nhiên, khi làm việc trong môi trường mà C là ngôn ngữ được ưu tiên hoặc yêu cầu - như hệ thống nhúng hoặc khi giao tiếp với thư viện C hiện có - việc sử dụng `jansson` hoặc `json-c` trở nên không thể thiếu.

Cũng đáng lưu ý rằng, làm việc với JSON trong C đòi hỏi phải hiểu sâu sắc về quản lý bộ nhớ, vì những thư viện này thường trả về các đối tượng được cấp phát động yêu cầu giải phóng rõ ràng. Điều này thách thức lập trình viên cân nhắc giữa tiện ích và trách nhiệm ngăn chặn rò rỉ bộ nhớ, một khía cạnh quan trọng trong việc tạo ra mã C hiệu quả.
