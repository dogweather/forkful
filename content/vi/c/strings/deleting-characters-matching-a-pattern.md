---
title:                "Xóa các ký tự khớp với một mô hình"
aliases: - /vi/c/deleting-characters-matching-a-pattern.md
date:                  2024-02-03T17:55:48.563411-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xóa các ký tự khớp với một mô hình"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Việc xóa các ký tự phù hợp với một mẫu cụ thể khỏi chuỗi trong C là để loại bỏ tất cả các trường hợp của những ký tự nào đó phù hợp với các tiêu chuẩn được định trước. Các lập trình viên thực hiện công việc này để làm sạch đầu vào, chuẩn bị dữ liệu cho quá trình xử lý, hoặc đơn giản là dọn dẹp chuỗi cho đầu ra hoặc thao tác tiếp theo, đảm bảo rằng dữ liệu được xử lý chính xác như cần thiết cho một ngữ cảnh hoặc thuật toán nhất định.

## Làm thế nào:

C không đi kèm với một hàm được xây dựng sẵn để trực tiếp xóa các ký tự của chuỗi dựa trên một mẫu, không giống như một số ngôn ngữ cấp cao. Tuy nhiên, bạn có thể dễ dàng thực hiện công việc này bằng cách lặp qua chuỗi một cách thủ công và xây dựng một chuỗi mới không bao gồm các ký tự không mong muốn. Chẳng hạn, giả sử bạn muốn loại bỏ tất cả các chữ số khỏi một chuỗi. Bạn có thể thực hiện như sau:

```c
#include <stdio.h>
#include <ctype.h>

void remove_digits(char *str) {
    char *src = str, *dst = str;
    while (*src) {
        if (!isdigit((unsigned char)*src)) {
            *dst++ = *src;
        }
        src++;
    }
    *dst = '\0';
}

int main() {
    char str[] = "C Programming 101: The Basics!";
    remove_digits(str);
    printf("Kết quả: %s\n", str);
    return 0;
}
```

Kết quả mẫu:
```
Kết quả: C Programming : The Basics!
```

Ví dụ này sử dụng `isdigit` từ `ctype.h` để xác định các chữ số, di chuyển các ký tự không phải là số đến đầu chuỗi và kết thúc chuỗi một khi tất cả các ký tự đã được đánh giá.

## Sâu hơn

Giải pháp được trình bày sử dụng một phương pháp với hai con trỏ trong cùng một mảng để lọc hiệu quả các ký tự không mong muốn, một kỹ thuật tượng trưng cho triết lý quản lý bộ nhớ chủ động của C. Phương pháp này hiệu quả bởi vì nó hoạt động tại chỗ, tránh được nhu cầu về việc cấp phát bộ nhớ bổ sung và do đó giảm thiểu chi phí.

Truyền thống, sự thiếu hụt các hàm xử lý chuỗi cấp cao trong C đã buộc các lập trình viên phải phát triển một sự hiểu biết sâu sắc về cách xử lý chuỗi ở cấp độ bộ nhớ, dẫn đến những phương pháp sáng tạo như trên. Mặc dù điều này có ưu điểm của việc kiểm soát tốt hơn và hiệu suất cao hơn, nhưng nó cũng đi kèm với rủi ro cao hơn liên quan đến các lỗi như tràn bộ đệm và lỗi off-by-one.

Trong những bối cảnh phát triển hiện đại, đặc biệt là những bối cảnh nhấn mạnh đến sự an toàn và bảo mật, các ngôn ngữ che giấu các thao tác cấp thấp như vậy có thể được ưa chuộng hơn cho các tác vụ xử lý chuỗi. Tuy nhiên, việc hiểu và sử dụng các kỹ thuật C này vẫn rất quý giá trong các tình huống đòi hỏi tối ưu hoá hiệu suất dưới số lượng nhỏ hoặc khi làm việc trong các môi trường mà sự tối giản và tốc độ của C là cực kỳ quan trọng.
