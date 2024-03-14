---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:02.522253-07:00
description: "YAML, vi\u1EBFt t\u1EAFt c\u1EE7a \"YAML Ain't Markup Language,\" l\xE0\
  \ m\u1ED9t chu\u1EA9n h\xF3a chu\u1ED7i d\u1EEF li\u1EC7u d\u1EC5 \u0111\u1ECDc\
  \ v\u1EDBi con ng\u01B0\u1EDDi, c\xF3 th\u1EC3 \u0111\u01B0\u1EE3c s\u1EED d\u1EE5\
  ng cho h\xE0ng lo\u1EA1t \u1EE9ng d\u1EE5ng, t\u1EEB t\u1EC7p\u2026"
lastmod: '2024-03-13T22:44:37.295823-06:00'
model: gpt-4-0125-preview
summary: "YAML, vi\u1EBFt t\u1EAFt c\u1EE7a \"YAML Ain't Markup Language,\" l\xE0\
  \ m\u1ED9t chu\u1EA9n h\xF3a chu\u1ED7i d\u1EEF li\u1EC7u d\u1EC5 \u0111\u1ECDc\
  \ v\u1EDBi con ng\u01B0\u1EDDi, c\xF3 th\u1EC3 \u0111\u01B0\u1EE3c s\u1EED d\u1EE5\
  ng cho h\xE0ng lo\u1EA1t \u1EE9ng d\u1EE5ng, t\u1EEB t\u1EC7p\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
---

{{< edit_this_page >}}

## Gì và Tại sao?

YAML, viết tắt của "YAML Ain't Markup Language," là một chuẩn hóa chuỗi dữ liệu dễ đọc với con người, có thể được sử dụng cho hàng loạt ứng dụng, từ tệp cấu hình đến lưu trữ dữ liệu. Lập trình viên thường làm việc với YAML khi họ cần một định dạng dễ đọc và dễ viết cho tệp cấu hình hoặc trao đổi dữ liệu giữa các ngôn ngữ và hệ thống.

## Cách làm:

Để làm việc với YAML trong C cần có một thư viện, vì thư viện chuẩn C không cung cấp hỗ trợ trực tiếp cho việc phân tích cú pháp hoặc chuỗi hóa YAML. Một trong những thư viện YAML phổ biến nhất cho C là `libyaml`, cung cấp cả giao diện cấp thấp và cấp cao để phân tích cú pháp và phát sinh YAML. Dưới đây là một ví dụ về cách phân tích một tệp YAML đơn giản sử dụng `libyaml`:

**Đầu tiên**, bạn cần cài đặt thư viện `libyaml`. Nếu bạn sử dụng hệ thống giống Unix, bạn có thể thường xuyên cài đặt nó qua trình quản lý gói. Chẳng hạn, trên Ubuntu:

```bash
sudo apt-get install libyaml-dev
```

**Tiếp theo**, xem xét một tệp YAML đơn giản có tên là `config.yaml`:

```yaml
name: John Doe
age: 29
married: false
```

**Dưới đây** là một ví dụ cơ bản về cách phân tích tệp YAML này trong C:

```c
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

void process_yaml_file(const char *filename) {
    FILE *fh = fopen(filename, "rb");
    yaml_parser_t parser;
    yaml_event_t event;

    if (!yaml_parser_initialize(&parser))
        fputs("Failed to initialize YAML parser!\n", stderr);

    if (fh == NULL)
        fputs("Failed to open file!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    while (1) {
        if (!yaml_parser_parse(&parser, &event))
            break;

        if (event.type == YAML_SCALAR_EVENT) {
            printf("Giá trị: %s\n", event.data.scalar.value);
        }

        if (event.type == YAML_STREAM_END_EVENT)
            break;

        yaml_event_delete(&event);
    }

    yaml_parser_delete(&parser);
    fclose(fh);
}

int main() {
    process_yaml_file("config.yaml");
    return 0;
}
```

Chương trình đơn giản này mở một tệp YAML, khởi tạo bộ phân tích cú pháp YAML và đọc tệp, in ra giá trị scalar (trong ví dụ này, các trường của YAML đơn giản của chúng tôi). Lưu ý rằng việc kiểm tra lỗi trong ví dụ đơn giản này là tối thiểu và cần được chú trọng hơn trong mã sản phẩm.

Chạy chương trình với `config.yaml` của chúng tôi sẽ cho ra kết quả:

```plaintext
Giá trị: John Doe
Giá trị: 29
Giá trị: false
```

## Sâu hơn

YAML được phát hành lần đầu vào năm 2001 và được thiết kế để dễ đọc và thân thiện với người dùng hơn các định dạng hàng chuỗi dữ liệu khác như XML hoặc JSON, vay mượn từ một số ngôn ngữ như C, Perl và Python cho triết lý thiết kế của mình. Mặc dù có lợi thế về độ dễ đọc và dễ chỉnh sửa bởi con người, YAML có thể phức tạp khi phân tích cú pháp một cách tự động do nó phụ thuộc vào thụt dòng và bộ tính năng rộng lớn của mình, bao gồm tham chiếu và các loại tùy chỉnh.

Mặc dù `libyaml` cung cấp quyền truy cập mạnh mẽ, cấp thấp để phân tích cú pháp và phát sinh YAML trong C, nó có thể gây rắc rối cho các nhiệm vụ đơn giản do API rườm rà của mình. Vì những lý do này, một số lập trình viên thích sử dụng các thư viện cấp cao hơn hoặc thậm chí các định dạng hàng chuỗi dữ liệu khác như JSON khi làm việc trong C, đặc biệt khi cần phân tích cú pháp hiệu suất cao với tải mã tối thiểu. Tuy nhiên YAML vẫn là lựa chọn phổ biến cho các tệp cấu hình và các tình huống mà độ dễ đọc bởi con người là quan trọng nhất. Các lựa chọn thay thế như TinyYAML hoặc nhúng một bộ thông dịch cấp cao (ví dụ, nhúng Python hoặc Lua) có thể cung cấp sự thuận tiện hơn cho các ứng dụng cụ thể, cân bằng giữa sự dễ sử dụng và nhu cầu về hiệu suất.
