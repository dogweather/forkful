---
title:                "Làm việc với XML"
aliases:
- /vi/c/working-with-xml.md
date:                  2024-02-03T18:13:37.501179-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với XML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/working-with-xml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Làm việc với XML trong C bao gồm việc phân tích cú pháp, truy vấn và thao tác với các tài liệu XML sử dụng các thư viện khác nhau. Các lập trình viên tương tác với XML do sự sử dụng rộng rãi của nó trong các dịch vụ web, các tệp cấu hình, và trao đổi dữ liệu giữa các hệ thống khác nhau, yêu cầu kỹ năng xử lý XML một cách hiệu quả cho phát triển ứng dụng mạnh mẽ.

## Làm thế nào:

C không có hỗ trợ sẵn cho XML, vì vậy bạn sẽ cần sử dụng các thư viện bên ngoài. Một lựa chọn phổ biến là `libxml2`, một thư viện ổn định và giàu tính năng. Dưới đây là cách đọc và phân tích cú pháp một tệp XML sử dụng `libxml2`.

Đầu tiên, đảm bảo bạn đã cài đặt `libxml2` trên hệ thống của mình. Bạn có thể cần phải cài đặt nó thông qua trình quản lý gói của mình (ví dụ: `apt-get install libxml2-dev` trên hệ thống Debian).

Tiếp theo, bao gồm tiêu đề `libxml2` trong chương trình C của bạn:

```c
#include <libxml/parser.h>
#include <libxml/tree.h>
```

Bây giờ, hãy viết một chương trình đơn giản để phân tích cú pháp một tệp XML và in ra tên của các phần tử cấp đầu tiên:

```c
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main(void) {
    xmlDoc *document = NULL;
    xmlNode *root_element = NULL;

    // Khởi tạo thư viện và kiểm tra không khớp ABI tiềm năng
    LIBXML_TEST_VERSION

    // Phân tích cú pháp tệp và lấy DOM
    document = xmlReadFile("your_file.xml", NULL, 0);

    if (document == NULL) {
        printf("Không thành công trong việc phân tích cú pháp tệp XML\n");
        return -1;
    }

    // Lấy nút phần tử gốc
    root_element = xmlDocGetRootElement(document);

    for (xmlNode *currentNode = root_element; currentNode; currentNode = currentNode->next) {
        if (currentNode->type == XML_ELEMENT_NODE) {
            printf("Loại Nút: Phần tử, tên: %s\n", currentNode->name);
        }
    }

    // Giải phóng bộ nhớ được cấp phát cho bộ phân tích cú pháp và DOM
    xmlFreeDoc(document);

    // Dọn dẹp và kiểm tra rò rỉ
    xmlCleanupParser();
    xmlMemoryDump(); // Tùy chọn

    return 0;
}
```

Để biên dịch chương trình này, hãy đảm bảo liên kết với `libxml2`:

```sh
gcc -o xml_example xml_example.c $(xml2-config --cflags --libs)
```

Giả sử bạn có một tệp XML có tên là `your_file.xml`, chạy chương trình đã biên dịch nên in ra tên của các phần tử cấp đầu tiên của nó.

## Hiểu sâu hơn

Sự tương tác giữa C và XML là câu chuyện về việc đưa hai thế giới cực kỳ khác biệt lại với nhau: mô hình cấu trúc, byte-level, quy trình của C và mô hình phân cấp, dài dòng, và tập trung vào tài liệu của XML. Khi tích hợp khả năng xử lý XML vào các chương trình C, các nhà phát triển tận dụng những điểm mạnh của C - như tốc độ và truy cập bộ nhớ cấp thấp - để phân tích cú pháp và thao tác hiệu quả các tài liệu XML.

`libxml2`, phát triển như một phần của dự án GNOME, đã trở nên chuẩn mực de facto cho việc xử lý XML trong C do sự hỗ trợ toàn diện cho các tiêu chuẩn XML và hiệu suất của nó. Nó thể hiện nhiều năm nỗ lực phát triển và đóng góp từ cộng đồng, làm cho nó vững chắc và hiệu quả cho hầu hết các tác vụ XML.

Mặc dù `libxml2` cung cấp các khả năng mạnh mẽ, cần lưu ý rằng sự phức tạp của việc phân tích cú pháp và thao tác XML có thể giới thiệu một khoản chi phí đáng kể. Trong các kịch bản mà sự dài dòng và phức tạp của XML là không thể biện minh, các phương thức thay thế như JSON có thể được ưa chuộng hơn cho trao đổi dữ liệu. Tuy nhiên, đối với các ứng dụng hoặc môi trường tập trung vào XML hoặc nơi sử dụng XML được ăn sâu, việc thành thạo sử dụng `libxml2` trong C mở ra khả năng làm việc với nhiều loại tài liệu và API XML, tạo cầu nối giữa ngôn ngữ lập trình C và thế giới xử lý tài liệu có cấu trúc.
