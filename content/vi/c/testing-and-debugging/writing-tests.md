---
title:                "Viết kiểm thử"
aliases: - /vi/c/writing-tests.md
date:                  2024-02-03T18:15:15.901214-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết kiểm thử"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Viết test trong C bao gồm việc tạo ra các chương trình phụ hoặc chức năng nhỏ hơn giúp tự động kiểm tra tính năng của mã lệnh. Lập trình viên làm điều này để đảm bảo phần mềm của họ hoạt động như mong đợi, bắt lỗi sớm và để thuận tiện cho việc chỉnh sửa mã sau này mà không gây ra hiệu ứng phụ không mong muốn.

## Làm thế nào:
Mặc dù C không có bộ khung test tích hợp sẵn như một số ngôn ngữ khác, bạn vẫn có thể viết các bài test hiệu quả sử dụng assert.h cho các phát biểu đơn giản hoặc tích hợp các bộ khung bên thứ ba như CUnit hoặc Unity cho việc test có cấu trúc hơn. Dưới đây là một ví dụ cơ bản sử dụng assert.h để test một hàm cộng hai số nguyên:

```c
#include <assert.h>
#include "my_math.h"

void test_addition() {
    assert(add(1, 2) == 3);
    assert(add(-1, -2) == -3);
    assert(add(0, 0) == 0);
    printf("Tất cả các bài test cộng đều đã vượt qua.\n");
}

int main() {
    test_addition();
    return 0;
}
```

Trong `my_math.h`, bạn có thể có:

```c
// Hàm cộng đơn giản
int add(int a, int b) {
    return a + b;
}
```

Chạy hàm test trong hàm `main` của bạn sẽ xuất ra:

```
Tất cả các bài test cộng đều đã vượt qua.
```

Đối với một bộ thiết lập test toàn diện hơn sử dụng một bộ khung như Unity, bạn sẽ tích hợp bộ khung vào dự án của mình, sau đó viết các trường hợp test tương tự, nhưng sử dụng API của bộ khung cho việc đưa ra khẳng định và chạy test.

## Sâu hơn
Việc test trong C từng là một quy trình thủ công và tương đối tự phát do bản chất cấp thấp của ngôn ngữ và thiếu một bộ khung test chuẩn hóa. Phương pháp thủ công này thường dẫn đến việc thực hành test không kỹ lưỡng so với các ngôn ngữ có hỗ trợ test tích hợp. Vì ngôn ngữ C đã có vai trò quan trọng trong việc phát triển các hệ thống phần mềm cơ bản, việc thiếu các bộ khung test chính thức đã thúc đẩy cộng đồng C phát triển các giải pháp bên thứ ba, như CUnit và Unity.

Những công cụ này, mặc dù nằm ngoài thư viện chuẩn của C, cung cấp chức năng tương tự như bộ khung test trong các ngôn ngữ khác, đề xuất một cách có cấu trúc để định nghĩa, chạy và đánh giá các bài test. Chúng giúp cầu nối giữa quyền truy cập cấp hệ thống mạnh mẽ của C và thực hành phát triển hiện đại của testing tự động. Đáng chú ý là trong khi những công cụ này nâng cao quy trình test trong C, chúng có thể giới thiệu một đường cong học tập và tăng độ phức tạp của việc thiết lập dự án so với các ngôn ngữ có hỗ trợ testing tích hợp. Do đó, đối với các dự án nơi độ tin cậy và khả năng bảo trì là quan trọng, việc đầu tư vào việc thiết lập môi trường test thích hợp trong C được biện minh chính đáng, ngay cả khi có sự xuất hiện của các lựa chọn thay thế.
