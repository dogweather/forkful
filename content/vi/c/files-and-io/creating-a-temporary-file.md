---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:15.625637-07:00
description: "L\xE0m th\u1EBF n\xE0o: Vi\u1EC7c t\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1\
  m th\u1EDDi trong ng\xF4n ng\u1EEF l\u1EADp tr\xECnh C c\xF3 th\u1EC3 t\u1EADn d\u1EE5\
  ng c\xE1c h\xE0m nh\u01B0 `tmpfile()` v\xE0 `mkstemp()`. **S\u1EED d\u1EE5ng `tmpfile()`**:\
  \ H\xE0m\u2026"
lastmod: '2024-03-13T22:44:37.294479-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi trong ng\xF4\
  n ng\u1EEF l\u1EADp tr\xECnh C c\xF3 th\u1EC3 t\u1EADn d\u1EE5ng c\xE1c h\xE0m nh\u01B0\
  \ `tmpfile()` v\xE0 `mkstemp()`."
title: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi"
weight: 21
---

## Làm thế nào:
Việc tạo một tập tin tạm thời trong ngôn ngữ lập trình C có thể tận dụng các hàm như `tmpfile()` và `mkstemp()`.

**Sử dụng `tmpfile()`**: Hàm này tạo ra một tập tin tạm thời duy nhất tự động được xóa khi chương trình kết thúc hoặc tập tin được đóng.

```c
#include <stdio.h>

int main() {
    FILE *temp = tmpfile();
    if (temp == NULL) {
        perror("Không thể tạo tập tin tạm thời");
        return 1;
    }

    // Viết dữ liệu vào tập tin tạm thời
    fputs("Đây là một bài kiểm tra.\n", temp);

    // Quay lại và đọc những gì chúng ta đã viết
    rewind(temp);
    char buffer[1024];
    while (fgets(buffer, sizeof(buffer), temp) != NULL) {
        printf("%s", buffer);
    }

    // Tự động xóa khi đóng hoặc kết thúc chương trình
    fclose(temp);

    return 0;
}
```
**Kết quả mẫu:**
```
Đây là một bài kiểm tra.
```

**Sử dụng `mkstemp()`**: Cung cấp nhiều quyền kiểm soát hơn về vị trí và quyền của tập tin tạm thời. Nó yêu cầu một chuỗi mẫu kết thúc với `XXXXXX` mà sau đó nó sẽ thay thế bằng một chuỗi duy nhất để ngăn chặn sự trùng lặp tên.

```c
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

int main() {
    char template[] = "/tmp/mytemp-XXXXXX";
    int fd = mkstemp(template);

    if (fd == -1) {
        perror("Không thể tạo tập tin tạm thời");
        return 1;
    }
    
    printf("Tập tin tạm thời được tạo: %s\n", template);

    // Các tập tin tạm thời được tạo bằng mkstemp() cần được xóa thủ công
    unlink(template);

    close(fd);
    return 0;
}
```
**Kết quả mẫu:**
```
Tập tin tạm thời được tạo: /tmp/mytemp-abc123
```

## Đào sâu
Khái niệm của tập tin tạm thời không phải là duy nhất trong C nhưng là một tính năng phổ biến trong nhiều môi trường lập trình do khả năng xử lý dữ liệu thoáng qua của nó. Hàm `tmpfile()`, được chuẩn hóa trong tiêu chuẩn ISO C, tạo một tập tin với một tên duy nhất trong một thư mục tiêu chuẩn, nhưng sự tồn tại của nó là phù duyện, khiến nó trở nên lý tưởng cho các thao tác an toàn hoặc tạm thời.

Một hạn chế đáng chú ý của `tmpfile()` là sự phụ thuộc vào thư mục tạm thời mặc định, có thể không phù hợp với tất cả các ứng dụng, đặc biệt là về mặt quyền hoặc an ninh. Ngược lại, `mkstemp()` cho phép chỉ định thư mục và đảm bảo việc tạo tập tin an toàn với các tên tập tin duy nhất được đảm bảo bằng cách sửa đổi chuỗi mẫu cung cấp, mang lại một giải pháp đa dạng hóa hơn ở chi phí quản lý tập tin thủ công.

Tuy nhiên, việc tạo tập tin tạm thời có thể giới thiệu các lỗ hổng bảo mật, chẳng hạn như tình trạng đua, nếu không được xử lý đúng cách. Ví dụ, `tmpfile()` và `mkstemp()` giải quyết các khía cạnh khác nhau của việc tạo tập tin tạm thời an toàn (xóa tự động và tạo tên an toàn, tương ứng), nhưng không có hàm nào là panacea. Các nhà phát triển phải xem xét cụ thể nhu cầu an ninh của ứng dụng của họ, bao gồm cả các lỗ hổng tiềm ẩn được giới thiệu bởi các tập tin tạm thời, và có thể cần phải triển khai các biện pháp phòng ngừa bổ sung ngoài những gì các hàm này cung cấp.

Trong bối cảnh rộng lớn hơn của lập trình, các phương án thay thế như lưu trữ trong bộ nhớ (ví dụ, sử dụng cấu trúc dữ liệu động hoặc tập tin ánh xạ bộ nhớ) có thể cung cấp hiệu suất hoặc bảo mật tốt hơn cho việc xử lý dữ liệu tạm thời. Tuy nhiên, các tập tin tạm thời vật lý vẫn là một công cụ quan trọng trong nhiều kịch bản, đặc biệt là đối với các bộ dữ liệu lớn hoặc khi liên lạc giữa các quá trình được liên quan.
