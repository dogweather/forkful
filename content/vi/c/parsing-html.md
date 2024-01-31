---
title:                "Phân Tích Cú Pháp HTML"
date:                  2024-01-28T22:03:49.264511-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân Tích Cú Pháp HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Phân tích cú pháp HTML có nghĩa là đọc và hiểu cấu trúc của các tài liệu HTML bằng chương trình. Lập trình viên thực hiện điều này để thao tác, trích xuất hoặc kiểm tra nội dung, thường xuyên khi vét các trang web hoặc xử lý dữ liệu web.

## Làm thế nào:

Được rồi, chúng ta hãy tới phần mã. C không có hỗ trợ tích hợp sẵn cho việc phân tích cú pháp HTML, vì vậy chúng ta sẽ sử dụng một thư viện gọi là Gumbo, đây là bộ phân tích cú pháp HTML5 thuần C. Dưới đây là một ví dụ nhanh:

```C
#include <stdio.h>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    GumboAttribute* href;
    if (node->v.element.tag == GUMBO_TAG_A &&
       (href = gumbo_get_attribute(&node->v.element.attributes, "href"))) {
        printf("Link found: %s\n", href->value);
    }
    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(children->data[i]);
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Example</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

Kết quả mẫu:

```
Link found: https://example.com
```

Ví dụ này tìm các thẻ 'a' và in ra các thuộc tính href. Hãy nhớ liên kết với gumbo (`gcc -o example example.c -lgumbo`) và cài đặt thư viện trước.

## Sâu hơn nữa

Câu chuyện về việc phân tích cú pháp HTML trong C khá gồ ghề. Không có giải pháp nào phù hợp với mọi trường hợp bởi vì HTML phức tạp và thường không nhất quán. Gumbo, mà chúng tôi đã sử dụng, được phát triển bởi Google như một phần của các dự án mã nguồn mở của họ. Nó được thiết kế để chịu đựng sự lộn xộn thực tế của các trang web.

Các lựa chọn thay thế bao gồm libxml2 với một chế độ phân tích cú pháp HTML, mặc dù nó lịch sử đã được liên kết nhiều hơn với việc phân tích cú pháp XML. Một lựa chọn khác là htmlcxx, thực sự là C++, nhưng chúng ta không lạc đề ở đây.

Về mặt hiệu suất, các bộ phân tích cú pháp C có thể nhanh chóng đáng kinh ngạc nhưng thường không cung cấp sự dễ sử dụng như các thư viện Python. Khi triển khai C cho việc phân tích cú pháp HTML, bạn có thể sau hiệu suất, hoặc bạn đang tích hợp nó vào một cơ sở mã C hiện có. Nó có thể khá tinh vi, vì hầu hết các thư viện C đều ở cấp độ thấp và cần thao tác nhiều hơn so với các bộ phân tích cú pháp Python hoặc JavaScript.

## Xem thêm

- Gumbo Parser: [https://github.com/google/gumbo-parser](https://github.com/google/gumbo-parser)
- libxml2 HTML parser: [http://xmlsoft.org/html/libxml-HTMLparser.html](http://xmlsoft.org/html/libxml-HTMLparser.html)
- htmlcxx: [http://htmlcxx.sourceforge.net/](http://htmlcxx.sourceforge.net/)
- Để bắt đầu nhẹ nhàng, hãy xem xét một hướng dẫn về việc vét trang web với Python sử dụng Beautiful Soup hoặc `html.parser` của Python như một giới thiệu dễ dàng hơn về chủ đề này.
