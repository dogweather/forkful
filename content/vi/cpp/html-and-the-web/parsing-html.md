---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:47.326286-07:00
description: "C\xE1ch th\u1EE9c: C++ kh\xF4ng \u0111i k\xE8m v\u1EDBi kh\u1EA3 n\u0103\
  ng ph\xE2n t\xEDch c\xFA ph\xE1p HTML \u0111\u01B0\u1EE3c t\xEDch h\u1EE3p s\u1EB5\
  n. B\u1EA1n th\u01B0\u1EDDng s\u1EBD s\u1EED d\u1EE5ng m\u1ED9t th\u01B0 vi\u1EC7\
  n nh\u01B0 Gumbo-parser c\u1EE7a Google, ho\u1EB7c\u2026"
lastmod: '2024-03-13T22:44:37.040174-06:00'
model: gpt-4-0125-preview
summary: "C++ kh\xF4ng \u0111i k\xE8m v\u1EDBi kh\u1EA3 n\u0103ng ph\xE2n t\xEDch\
  \ c\xFA ph\xE1p HTML \u0111\u01B0\u1EE3c t\xEDch h\u1EE3p s\u1EB5n."
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
weight: 43
---

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
