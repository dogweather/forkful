---
title:                "Đọc một tệp văn bản"
date:                  2024-01-28T22:05:15.539292-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc một tệp văn bản"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Đọc một tệp văn bản là về truy cập dữ liệu của tệp dưới dạng nội dung chuỗi, từng ký tự một hoặc từng dòng một. Lập trình viên làm điều này để xử lý, phân tích, hoặc thao tác thông tin đã lưu trữ mà không cần nhập liệu thủ công mỗi lần chạy.

## Làm thế nào:

Hãy đọc một tệp văn bản. Chúng ta sẽ mở nó, đọc từ nó và đóng nó. Cơ bản thôi.

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file;
    char filename[] = "example.txt";
    char ch;

    file = fopen(filename, "r"); // Mở tệp ở chế độ đọc

    if (file == NULL) {
        perror("Lỗi khi mở tệp.\n");
        exit(EXIT_FAILURE);
    }

    printf("Nội dung của %s:\n", filename);

    while ((ch = fgetc(file)) != EOF) { // Đọc và in từng ký tự
        putchar(ch);
    }

    fclose(file); // Đóng tệp

    return 0;
}
```

Giả sử `example.txt` chứa "Hello, C!", đầu ra sẽ là:
```
Nội dung của example.txt:
Hello, C!
```

## Sâu hơn nữa

Trở lại những năm 70, C được ra đời, và cùng với nó, cách chúng ta đọc tệp ngày nay. Đó không phải là khoa học phức tạp, nhưng có những sắc thái. Bạn sử dụng `fopen` để mở tệp và `fgetc` để đọc từng ký tự một lúc. Nhưng tại sao là từng ký tự một? Bạn có thể đọc từng dòng với `fgets` hoặc toàn bộ tệp với `fread` nếu nó phù hợp với trường hợp của bạn. Đó tất cả là về sự kiểm soát và những gì chương trình của bạn cần.

Đằng sau cánh gà, `fopen` nói với hệ điều hành của bạn, "Này, tôi sẽ cần tệp này, cho tôi quyền truy cập nhé!" Và hệ thống trả lời okay bằng cách trả lại một con trỏ `FILE`. Hàm `fgetc` thì thầm với con trỏ tệp, "Cho tôi byte tiếp theo, nhé?" Và nó làm vậy, cho đến khi nó đạt đến EOF, dấu hiệu Kết thúc Tệp.

Có lựa chọn khác? Chắc chắn rồi. Bạn có ` fscanf` cho đọc định dạng, `getline` cho các chàng trai hiện đại, hoặc lệnh gọi hệ thống `read` cấp thấp nếu bạn muốn gần gũi với phần cứng. Và đừng quên, sau khi byte cuối cùng được đọc, hãy lịch sự và `fclose` tệp.

## Xem thêm

Để tìm hiểu sâu hơn, hãy kiểm tra những cái này:

- Tài liệu Thư viện Chuẩn C: [https://en.cppreference.com/w/c/io](https://en.cppreference.com/w/c/io)
- Hướng dẫn Tham khảo Thư viện GNU C: [https://www.gnu.org/software/libc/manual/html_node/I_002fO-Overview.html](https://www.gnu.org/software/libc/manual/html_node/I_002fO-Overview.html)
- Tìm hiểu thêm về các hàm đọc khác nhau: [https://www.tutorialspoint.com/c_standard_library/c_function_fread.htm](https://www.tutorialspoint.com/c_standard_library/c_function_fread.htm)
- Dành cho những người thực sự tò mò, sâu hơn vào các lệnh gọi hệ thống Linux: [https://man7.org/linux/man-pages/man2/read.2.html](https://man7.org/linux/man-pages/man2/read.2.html)
