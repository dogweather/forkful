---
title:                "Viết một tệp văn bản"
aliases:
- /vi/c/writing-a-text-file/
date:                  2024-02-03T18:14:58.531147-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Việc viết một tệp văn bản trong C bao gồm việc tạo hoặc mở một tệp ở chế độ viết và sau đó sử dụng các hàm I/O tệp của C để lưu dữ liệu văn bản vào đó. Lập trình viên làm điều này để lưu trữ dữ liệu, như sự kiện nhật ký, cài đặt cấu hình, hoặc nội dung do người dùng tạo, cho phép ứng dụng duy trì trạng thái, sở thích, hoặc tiến trình của người dùng qua các phiên làm việc.

## Cách thực hiện:

Để viết văn bản vào một tệp trong C, bạn chủ yếu cần làm quen với các hàm `fopen()`, `fprintf()`, `fputs()`, và `fclose()`. Dưới đây là một ví dụ đơn giản minh họa việc tạo và viết vào một tệp:

```c
#include <stdio.h>

int main() {
    FILE *filePointer;
    // Mở một tệp ở chế độ viết. Nếu tệp không tồn tại, nó sẽ được tạo.
    filePointer = fopen("example.txt", "w");
    
    if(filePointer == NULL) {
        printf("Không thể mở tệp\n");
        return 1; // Chương trình thoát nếu con trỏ tệp trả về NULL.
    }
    
    // Viết vào tệp
    fprintf(filePointer, "Đây là một ví dụ về viết vào tệp.\n");
    fputs("Đây là một dòng văn bản khác.\n", filePointer);
    
    // Đóng tệp để lưu các thay đổi
    fclose(filePointer);
    
    printf("Tệp đã được viết thành công\n");
    return 0;
}
```

Kết quả mẫu khi thực thi thành công:
```
Tệp đã được viết thành công
```

Sau khi chạy chương trình này, bạn sẽ tìm thấy một tệp có tên `example.txt` trong cùng thư mục, chứa văn bản bạn đã viết qua `fprintf()` và `fputs()`.

## Sâu hơn

Khái niệm về tệp và hệ thống tệp đã là cơ bản cho các hệ thống máy tính, với việc quản lý chúng là một khía cạnh quan trọng của hệ điều hành. Trong C, việc xử lý tệp được thực hiện thông qua một bộ các hàm thư viện I/O tiêu chuẩn, dựa trên triết lý xử lý tệp như các dòng byte. Sự trừu tượng này cho phép một phương pháp đơn giản và hiệu quả để đọc từ và viết vào tệp, mặc dù nó có thể có vẻ cấp thấp so với các cách tiếp cận hiện đại hơn có sẵn trong các ngôn ngữ cấp cao như Python hay Ruby.

Lịch sử, các hoạt động I/O tệp trong C đã đặt nền móng cho việc thao tác tệp trong nhiều ngôn ngữ lập trình, cung cấp một giao diện gần với hệ điều hành hệ thống quản lý tệp. Điều này không chỉ cung cấp kiểm soát cụ thể về các thuộc tính tệp và hoạt động I/O mà còn đặt ra các rủi ro cho những lập trình viên không cảnh giác, như cần quản lý tài nguyên thủ công (ví dụ, luôn phải đóng các tệp) và các vấn đề về đệm.

Mặc dù các hàm I/O tệp cơ bản trong C là mạnh mẽ và đủ cho nhiều nhiệm vụ, chúng thiếu đi sự tiện lợi và các trừu tượng cấp cao mà các ngôn ngữ hiện đại cung cấp. Ngôn ngữ như Python tự động quản lý bộ nhớ và đóng tệp (sử dụng các câu lệnh `with`), giảm đáng kể mã boilerplate và rủi ro của rò rỉ tài nguyên. Đối với các ứng dụng cần thao tác tệp phức tạp hoặc các trừu tượng cấp cao hơn (như khóa tệp, I/O bất đồng bộ, hoặc theo dõi các sự kiện hệ thống tệp), việc tìm kiếm các thư viện cung cấp các tính năng này hoặc chọn một ngôn ngữ hỗ trợ các cấu trúc như vậy có lẽ là lựa chọn tốt hơn.

Tuy nhiên, việc hiểu biết về I/O tệp trong C là vô giá, cung cấp cái nhìn sâu sắc vào cơ sở của cách các ngôn ngữ cấp cao triển khai các tính năng này và cung cấp các công cụ để viết mã cấp thấp hiệu quả khi hiệu suất và kiểm soát là quan trọng nhất.
