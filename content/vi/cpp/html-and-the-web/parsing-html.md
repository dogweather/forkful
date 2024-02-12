---
title:                "Phân Tích Cú Pháp HTML"
aliases:
- /vi/cpp/parsing-html.md
date:                  2024-01-28T22:03:47.326286-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân Tích Cú Pháp HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Điều gì & Tại sao?
Phân tích cú pháp HTML có nghĩa là phá vỡ nội dung HTML thành cái gì đó mà chương trình có thể hiểu và thao tác được. Lập trình viên làm điều này để trích xuất dữ liệu, thao tác nội dung hoặc tích hợp việc quét web vào ứng dụng của họ.

## Cách thức:
C++ không đi kèm với khả năng phân tích cú pháp HTML được tích hợp sẵn. Bạn thường sẽ sử dụng một thư viện như Gumbo-parser của Google, hoặc một cái gì đó tương tự. Dưới đây là một ví dụ nhanh sử dụng Gumbo-parser:

```C++
#include <iostream>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    if (node->v.element.tag == GUMBO_TAG_A) {
        GumboAttribute* href = gumbo_get_attribute(&node->v.element.attributes, "href");
        if (href) {
            std::cout << href->value << std::endl;
        }
    }
    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(static_cast<GumboNode*>(children->data[i]));
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Liên kết</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

Kết quả mẫu:
```
https://example.com
```

## Sâu hơn
Việc phân tích cú pháp HTML không luôn luôn là điều dễ dàng trong C++. Trước đây, các lập trình viên sẽ sử dụng regex hoặc các trình phân tích cú pháp do chính họ viết, cả hai đều dễ phạm lỗi và cồng kềnh. Ngày nay, các thư viện mạnh mẽ như Gumbo-parser xử lý những phức tạp của việc phân tích cú pháp, làm cho nó dễ dàng và đáng tin cậy hơn.

Các lựa chọn thay thế bao gồm Tidy, MyHTML, hoặc thậm chí tích hợp C++ với BeautifulSoup của Python thông qua hàm `system` của C++ hoặc các bộ thông dịch nhúng.

Về mặt triển khai, các thư viện này chuyển đổi HTML thành một cây Mô hình Đối tượng Tài liệu (DOM). Duyệt và thao tác DOM cho phép người dùng trích xuất và làm việc với dữ liệu như đã trình bày trong phần Cách thức.

## Xem thêm
- [Kho lưu trữ GitHub của Gumbo-parser](https://github.com/google/gumbo-parser)
- [Danh sách các thư viện phân tích cú pháp HTML](https://en.cppreference.com/w/c/experimental/dynamic)
- [Tương tác giữa C++ và Python](https://docs.python.org/3/extending/embedding.html)
