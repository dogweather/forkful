---
title:                "Nối chuỗi ký tự"
aliases:
- vi/c/concatenating-strings.md
date:                  2024-02-03T17:54:16.050761-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nối chuỗi ký tự"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/concatenating-strings.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì và Tại Sao?

Nối chuỗi trong C bao gồm việc ghép hai hoặc nhiều chuỗi lại với nhau để tạo thành một chuỗi mới. Các lập trình viên thực hiện thao tác này để xây dựng chuỗi một cách động tại thời gian chạy, rất quan trọng cho việc tạo ra các thông điệp ý nghĩa, đường dẫn tệp, hoặc bất kỳ dữ liệu nào được tổ hợp từ các nguồn chuỗi khác nhau.

## Cách thực Hiện:

Trong C, chuỗi là mảng của các ký tự kết thúc bằng ký tự null (`\0`). Không giống như trong các ngôn ngữ cấp cao hơn, C không cung cấp một hàm nối chuỗi tích hợp. Thay vào đó, bạn sử dụng hàm `strcat()` hoặc `strncat()` từ thư viện `<string.h>`.

Dưới đây là một ví dụ đơn giản sử dụng `strcat()`:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hello, ";
    char source[] = "World!";

    strcat(destination, source);

    printf("%s\n", destination);  // Đầu ra: Hello, World!
    return 0;
}
```

Hàm `strcat()` nhận hai đối số: chuỗi đích (phải có đủ không gian để chứa kết quả nối chuỗi) và chuỗi nguồn. Nó sau đó sẽ thêm chuỗi nguồn vào sau chuỗi đích.

Để kiểm soát chặt chẽ hơn số lượng ký tự được nối, `strncat()` an toàn hơn để sử dụng:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hello, ";
    char source[] = "World!";
    int num = 3; // Số lượng ký tự để thêm vào

    strncat(destination, source, num);

    printf("%s\n", destination);  // Đầu ra: Hello, Wor
    return 0;
}
```

Điều này giới hạn việc nối chuỗi chỉ với `num` ký tự đầu tiên của chuỗi nguồn, giúp ngăn chặn tràn bộ đệm.

## Sâu hơn nữa

Các hàm `strcat()` và `strncat()` đã là một phần của thư viện chuẩn C kể từ khi ngôn ngữ này được ra đời, phản ánh bản chất cấp thấp của ngôn ngữ yêu cầu quản lý thủ công chuỗi và bộ nhớ. Không giống như nhiều ngôn ngữ lập trình hiện đại xem chuỗi như các đối tượng hàng đầu với các toán tử nối chuỗi tích hợp sẵn (như `+` hoặc `.concat()`), cách tiếp cận của C yêu cầu hiểu biết sâu sắc hơn về con trỏ, cấp phát bộ nhớ và những rủi ro tiềm ẩn như tràn bộ đệm.

Mặc dù `strcat()` và `strncat()` được sử dụng rộng rãi, chúng thường xuyên bị chỉ trích vì những lỗ hổng bảo mật tiềm ẩn nếu không được sử dụng cẩn thận. Tràn bộ đệm, nơi dữ liệu vượt quá bộ nhớ được cấp phát, có thể dẫn đến sự cố hệ thống hoặc bị khai thác để thực thi mã tuỳ ý. Kết quả là, các lập trình viên ngày càng chuyển sang sử dụng các phương án thay thế an toàn hơn, như `snprintf()`, cung cấp hành vi dễ dự đoán hơn bằng cách giới hạn số lượng ký tự được ghi vào chuỗi đích dựa trên kích thước của nó:

```c
char destination[50] = "Hello, ";
char source[] = "World!";
snprintf(destination + strlen(destination), sizeof(destination) - strlen(destination), "%s", source);
```

Phương pháp này dài dòng hơn nhưng đáng kể an toàn hơn, làm nổi bật sự chuyển dịch trong thực hành lập trình C về ưu tiên an ninh và độ bền hơn là sự ngắn gọn.

Mặc dù có những thách thức này, nối chuỗi trong C là một kỹ năng cơ bản, quan trọng cho việc lập trình hiệu quả trong ngôn ngữ này. Hiểu rõ những tinh tế và rủi ro liên quan là chìa khóa để thành thạo lập trình C.
