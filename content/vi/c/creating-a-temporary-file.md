---
title:                "Tạo một tập tin tạm thời"
date:                  2024-01-28T21:58:31.451412-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tạo một tập tin tạm thời"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?
Việc tạo một tập tin tạm thời trong C cung cấp một bảng nháp cho việc xử lý dữ liệu. Đây là cách để lưu trữ dữ liệu mà bạn cần trong quá trình thực thi chương trình nhưng không cần sau khi chương trình kết thúc.

## Cách Thực Hiện:

C có các hàm như `tmpfile()` và `mkstemp()` để tạo tập tin tạm. Dưới đây là ví dụ về `tmpfile()`:

```c
#include <stdio.h>

int main() {
    FILE *temp = tmpfile();
    if (temp) {
        fputs("Write something temporary.", temp);
        // Sử dụng tập tin...
        rewind(temp); // Quay lại đầu tập tin để đọc những gì chúng ta đã viết.
        
        // Giả sử chúng ta muốn hiển thị nó:
        char buffer[100];
        while (fgets(buffer, sizeof(buffer), temp) != NULL) {
            printf("%s", buffer);
        }
        // Đóng và tự động xóa khi chương trình kết thúc
        fclose(temp);
    } else {
        perror("tmpfile() failed");
    }

    return 0;
}
```
Kết quả mẫu: `Write something temporary.`

## Sâu Hơn
Tập tin tạm đã có từ khi hệ điều hành hiện đại xuất hiện. Chúng hữu ích cho việc xử lý dữ liệu lớn không vừa với bộ nhớ, cho giao tiếp giữa các quy trình, hoặc cho bảo mật (vì chúng thường bị xóa khi chương trình kết thúc).

`tmpfile()` tạo một tập tin tạm thời duy nhất trong chế độ đọc/ghi nhị phân (`w+b`). Tập tin sẽ tự động bị xóa khi nó được đóng hoặc chương trình kết thúc. Chỉ nhớ, vì tập tin được mở trong chế độ nhị phân, nếu bạn đang xử lý văn bản, việc chuyển đổi các ký tự xuống dòng sẽ không được xử lý tự động.

Nếu bạn cần kiểm soát nhiều hơn, hãy sử dụng `mkstemp()`. Nó thay thế các ký tự mẫu trong tên tập tin của bạn bằng một chuỗi duy nhất, và bạn phải tự xóa tập tin khi hoàn thành.

```c
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

int main() {
    char template[] = "/tmp/mytemp.XXXXXX";
    int fd = mkstemp(template);
    if (fd == -1) {
        perror("mkstemp() failed");
        exit(EXIT_FAILURE);
    }

    // Chuyển đổi file descriptor thành đối tượng FILE
    FILE *temp = fdopen(fd, "w+");
    if (temp == NULL) {
        perror("fdopen() failed");
        close(fd);
        exit(EXIT_FAILURE);
    }

    fputs("Here's to more control over temp files.", temp);
    
    // Dọn dẹp: Đóng và xóa thủ công
    fclose(temp); 
    unlink(template); // Xóa tập tin

    return 0;
}
```
Kết quả mẫu: (Không có kết quả rõ ràng, nhưng một tập tin tạm được tạo và xóa)

Tại sao không tự tạo tập tin tạm của riêng bạn bằng cách dùng `fopen()`? Rủi ro về sự va chạm. Nhớ lại, `tmpfile()` và `mkstemp()` đảm bảo tên tập tin là duy nhất để tránh xung đột.

## Xem Thêm

- Tài liệu Thư viện Chuẩn C: https://en.cppreference.com/w/c/io
- Hướng dẫn sử dụng Thư viện C của GNU cho Giao diện Hệ thống Tập tin: https://www.gnu.org/software/libc/manual/html_node/File-System-Interface.html
- Viết mã an toàn trong C và C++ cho việc xử lý tập tin và dữ liệu một cách an toàn: https://www.securecoding.cert.org
