---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:24.092708-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 s\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9\
  c ch\xEDnh quy trong C, b\u1EA1n ch\u1EE7 y\u1EBFu s\u1EBD l\xE0m vi\u1EC7c v\u1EDB\
  i th\u01B0 vi\u1EC7n regex POSIX (`<regex.h>`). V\xED d\u1EE5 sau \u0111\xE2y minh\
  \ h\u1ECDa kh\u1EDBp m\u1EABu c\u01A1\u2026"
lastmod: '2024-03-13T22:44:37.254033-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 s\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy trong C,\
  \ b\u1EA1n ch\u1EE7 y\u1EBFu s\u1EBD l\xE0m vi\u1EC7c v\u1EDBi th\u01B0 vi\u1EC7\
  n regex POSIX (`<regex.h>`)."
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

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
