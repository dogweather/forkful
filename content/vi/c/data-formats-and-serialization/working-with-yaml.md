---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:02.522253-07:00
description: "C\xE1ch l\xE0m: \u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi YAML trong C\
  \ c\u1EA7n c\xF3 m\u1ED9t th\u01B0 vi\u1EC7n, v\xEC th\u01B0 vi\u1EC7n chu\u1EA9\
  n C kh\xF4ng cung c\u1EA5p h\u1ED7 tr\u1EE3 tr\u1EF1c ti\u1EBFp cho vi\u1EC7c ph\xE2\
  n t\xEDch c\xFA ph\xE1p ho\u1EB7c chu\u1ED7i h\xF3a\u2026"
lastmod: '2024-03-13T22:44:37.295823-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi YAML trong C c\u1EA7n c\xF3 m\u1ED9\
  t th\u01B0 vi\u1EC7n, v\xEC th\u01B0 vi\u1EC7n chu\u1EA9n C kh\xF4ng cung c\u1EA5\
  p h\u1ED7 tr\u1EE3 tr\u1EF1c ti\u1EBFp cho vi\u1EC7c ph\xE2n t\xEDch c\xFA ph\xE1\
  p ho\u1EB7c chu\u1ED7i h\xF3a YAML."
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

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
