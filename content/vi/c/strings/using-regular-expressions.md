---
title:                "Sử dụng biểu thức chính quy"
aliases: - /vi/c/using-regular-expressions.md
date:                  2024-02-03T18:11:24.092708-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng biểu thức chính quy"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Biểu thức chính quy (regex) cung cấp một cách để tìm kiếm, khớp và thao tác với các chuỗi sử dụng các mẫu đã định nghĩa. Lập trình viên sử dụng chúng một cách rộng rãi cho các nhiệm vụ như xác thực đầu vào, phân tích dữ liệu văn bản, và tìm kiếm các mô hình trong các tệp văn bản lớn, làm cho chúng trở thành một công cụ mạnh mẽ trong bất kỳ ngôn ngữ nào, bao gồm cả C.

## Làm thế nào:

Để sử dụng biểu thức chính quy trong C, bạn chủ yếu sẽ làm việc với thư viện regex POSIX (`<regex.h>`). Ví dụ sau đây minh họa khớp mẫu cơ bản:

```c
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main(){
    regex_t regex;
    int return_value;
    char *pattern = "^a[[:alnum:]]"; // Mẫu để khớp với các chuỗi bắt đầu bằng 'a' theo sau là các ký tự chữ số
    char *test_string = "apple123";

    // Biên dịch biểu thức chính quy
    return_value = regcomp(&regex, pattern, REG_EXTENDED);
    if (return_value) {
        printf("Could not compile regex\n");
        exit(1);
    }

    // Thực thi biểu thức chính quy
    return_value = regexec(&regex, test_string, 0, NULL, 0);
    if (!return_value) {
        printf("Match found\n");
    } else if (return_value == REG_NOMATCH) {
        printf("No match found\n");
    } else {
        printf("Regex match failed\n");
        exit(1);
    }

    // Giải phóng bộ nhớ đã cấp phát cho regex
    regfree(&regex);

    return 0;
}
```

Kết quả cho một chuỗi khớp ("apple123"):
```
Match found
```
Và cho một chuỗi không khớp ("banana"):
```
No match found
```

## Sâu hơn:

Biểu thức chính quy trong C, như là một phần của tiêu chuẩn POSIX, cung cấp một cách mạnh mẽ để thực hiện việc khớp và thao tác chuỗi. Tuy nhiên, API của thư viện regex POSIX trong C được coi là cồng kềnh hơn so với những ngôn ngữ được thiết kế với các tính năng thao tác chuỗi hạng nhất như Python hoặc Perl. Cú pháp cho các mẫu là tương tự nhau giữa các ngôn ngữ, nhưng C yêu cầu quản lý bộ nhớ thủ công và nhiều mã khung hơn để chuẩn bị, thực thi, và dọn dẹp sau khi sử dụng các mẫu regex.

Mặc dù có những thách thức này, việc học cách sử dụng regex trong C là có giá trị bởi vì nó làm sâu sắc thêm hiểu biết về các khái niệm lập trình cấp thấp. Ngoài ra, nó mở ra khả năng cho lập trình C trong các lĩnh vực như xử lý văn bản và trích xuất dữ liệu nơi mà regex là không thể thay thế. Đối với các mẫu phức tạp hơn hoặc các hoạt động regex, các lựa chọn thay thế như thư viện PCRE (Perl Compatible Regular Expressions) có thể cung cấp một giao diện phong phú tính năng hơn và phần nào dễ dàng hơn, mặc dù nó đòi hỏi việc tích hợp một thư viện bên ngoài vào dự án C của bạn.
