---
title:                "Làm việc với YAML"
date:                  2024-01-28T22:12:15.498749-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với YAML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

YAML là một định dạng tuần tự hóa dữ liệu dễ đọc dành cho con người, được sử dụng cho các tệp cấu hình, trao đổi dữ liệu giữa các ngôn ngữ, và lưu trữ dữ liệu. Các lập trình viên chọn YAML vì sự đơn giản và dễ đọc của nó, giúp việc sử dụng cho cấu hình nhanh và các tác vụ phát triển trở nên dễ dàng.

## Làm thế nào:

C không có chức năng phân tích cú pháp YAML tích hợp, do đó chúng ta sử dụng một thư viện như `libyaml` để xử lý các tệp YAML. Dưới đây là một ví dụ đơn giản về phân tích cú pháp một tệp YAML trong C.

Đầu tiên, bao gồm thư viện:
```C
#include <yaml.h>
```

Tiếp theo, khởi tạo một trình phân tích cú pháp, mở một tệp, và bắt đầu phân tích cú pháp:
```C
FILE *fh = fopen("config.yaml", "r");
yaml_parser_t parser;
yaml_parser_initialize(&parser);
yaml_parser_set_input_file(&parser, fh);

yaml_event_t event;
/* Đọc chuỗi sự kiện */
while (true) {
    if (!yaml_parser_parse(&parser, &event)) {
        printf("Lỗi phân tích cú pháp %d\n", parser.error);
        exit(EXIT_FAILURE);
    }

    if (event.type == YAML_SCALAR_EVENT) {
        printf("Nhận scalar (giá trị): %s\n", event.data.scalar.value);
    }

    if (event.type == YAML_STREAM_END_EVENT) {
        break;
    }

    yaml_event_delete(&event);
}

/* Dọn dẹp */
yaml_parser_delete(&parser);
fclose(fh);
```

Nội dung mẫu `config.yaml`:
```yaml
name: John Doe
age: 30
```

Đầu ra mẫu:
```
Nhận scalar (giá trị): name
Nhận scalar (giá trị): John Doe
Nhận scalar (giá trị): age
Nhận scalar (giá trị): 30
```

## Tìm hiểu sâu

YAML có nghĩa là "YAML Ain't Markup Language" (YAML không phải là ngôn ngữ đánh dấu). Nó xuất hiện vào đầu những năm 2000 như một lựa chọn thay thế cho XML đối với các tệp cấu hình, nhằm mục đích dễ đọc cho con người. YAML được sử dụng trong nhiều công cụ (như Docker, Kubernetes, v.v.) và thường được ưu tiên hơn JSON cho cấu hình do hỗ trợ các bình luận và cú pháp sạch sẽ hơn.

Các lựa chọn phổ biến trong C khi làm việc với YAML là `libyaml` và `yaml-cpp` (mặc dù cái sau là cho C++). Những thư viện này cho phép các chương trình C/C++ tuần tự hóa và giải tuần tự hóa dữ liệu YAML.

Khi phân tích cú pháp YAML, chương trình của bạn tạo một cây trong bộ nhớ. Các nút trong cây này có thể là ánh xạ (như từ điển hoặc bảng băm), chuỗi (như mảng), hoặc scalar (chuỗi, số, v.v.). Trình phân tích cú pháp của libyaml là dựa trên sự kiện, có nghĩa là nó đọc dòng YAML và phát ra các sự kiện cho mỗi cấu trúc YAML gặp phải. Xử lý những sự kiện này cho phép bạn xây dựng hoặc vận hành trên cấu trúc dữ liệu tương ứng.

## Xem thêm

- `libyaml` GitHub: https://github.com/yaml/libyaml
- Quy định chính thức của YAML: https://yaml.org/spec/1.2/spec.html
- Hướng dẫn "Lập trình với libyaml": https://libyaml.docsforge.com/master/programming-with-libyaml/
- So sánh các định dạng tuần tự hóa dữ liệu: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
