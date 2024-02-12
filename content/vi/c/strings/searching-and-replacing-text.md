---
title:                "Tìm kiếm và thay thế văn bản"
date:                  2024-02-03T18:08:50.114717-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm kiếm và thay thế văn bản"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/searching-and-replacing-text.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Tìm kiếm và thay thế văn bản trong ngôn ngữ C liên quan đến việc xác định các chuỗi con cụ thể trong một chuỗi lớn hơn và thay thế chúng bằng các chuỗi con khác. Lập trình viên thực hiện các thao tác này để thao tác dữ liệu văn bản - cho các công việc từ làm sạch dữ liệu và định dạng đến tạo nội dung động.

## Làm thế nào:

C không đi kèm với các hàm tích hợp sẵn để thực hiện trực tiếp việc tìm kiếm và thay thế trên chuỗi. Tuy nhiên, bạn có thể thực hiện điều này bằng cách kết hợp các hàm xử lý chuỗi có sẵn trong thư viện `<string.h>` cùng với một số logic tùy chỉnh. Dưới đây là một ví dụ cơ bản về cách tìm kiếm một chuỗi con trong một chuỗi và thay thế nó. Để đơn giản, ví dụ này giả sử kích thước bộ đệm đủ lớn và không xử lý các vấn đề về phân bổ bộ nhớ, điều mà bạn nên xem xét trong mã sản xuất.

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void replaceSubstring(char *source, char *sub, char *new_sub) {
    char buffer[1024];
    char *insert_point = &buffer[0];
    const char *tmp = source;
    size_t len_sub = strlen(sub), len_new_sub = strlen(new_sub);
    size_t len_up_to_match;

    while ((tmp = strstr(tmp, sub))) {
        // Tính toán chiều dài đến vị trí trùng khớp
        len_up_to_match = tmp - source;
        
        // Sao chép phần trước vị trí trùng khớp
        memcpy(insert_point, source, len_up_to_match);
        insert_point += len_up_to_match;
        
        // Sao chép chuỗi con mới
        memcpy(insert_point, new_sub, len_new_sub);
        insert_point += len_new_sub;
        
        // Di chuyển qua vị trí trùng khớp trong chuỗi nguồn
        tmp += len_sub;
        source = tmp;
    }
    
    // Sao chép bất kỳ phần còn lại nào của chuỗi nguồn
    strcpy(insert_point, source);
    
    // In chuỗi đã chỉnh sửa
    printf("Chuỗi đã chỉnh sửa: %s\n", buffer);
}

int main() {
    char sourceStr[] = "Xin chào, đây là một bài kiểm tra. Bài kiểm tra này đơn giản.";
    char sub[] = "kiểm tra";
    char newSub[] = "mẫu";
    
    replaceSubstring(sourceStr, sub, newSub);
    
    return 0;
}
```

Đầu ra mẫu:
```
Chuỗi đã chỉnh sửa: Xin chào, đây là một mẫu. Mẫu này đơn giản.
```

Mã này minh họa một cách tiếp cận đơn giản để tìm kiếm tất cả các trường hợp của một chuỗi con (`sub`) trong một chuỗi nguồn và thay thế chúng bằng một chuỗi con khác (`newSub`), sử dụng hàm `strstr` để tìm điểm bắt đầu của mỗi trùng khớp. Đây là một ví dụ rất cơ bản không xử lý các trường hợp phức tạp như chuỗi con chồng chéo.

## Đi sâu hơn

Cách tiếp cận được sử dụng trong phần "Làm thế nào" là cơ bản, minh họa cách thực hiện tìm kiếm và thay thế văn bản trong C mà không cần bất kỳ thư viện bên thứ ba nào. Lịch sử, do C tập trung vào quản lý bộ nhớ cấp thấp và hiệu suất, thư viện chuẩn của nó không bao gồm các chức năng thao tác chuỗi cấp cao như những ngôn ngữ như Python hay JavaScript. Lập trình viên phải tự quản lý bộ nhớ và kết hợp các thao tác chuỗi khác nhau để đạt được kết quả mong muốn, tăng cường độ phức tạp nhưng cung cấp nhiều kiểm soát và hiệu quả hơn.

Quan trọng là phải lưu ý rằng cách tiếp cận thủ công này có thể dễ mắc lỗi, đặc biệt khi quản lý phân bổ bộ nhớ và kích thước bộ đệm. Sự xử lý không chính xác có thể dẫn đến tràn bộ đệm và hỏng bộ nhớ, khiến mã nguồn dễ bị rủi ro về bảo mật.

Trong nhiều tình huống thực tế, đặc biệt là những tình huống đòi hỏi xử lý văn bản phức tạp, thường xem xét tích hợp thư viện bên thứ ba như PCRE (Perl Compatible Regular Expressions) cho tìm kiếm và thay thế dựa trên regex, có thể làm đơn giản hoá mã và giảm thiểu khả năng mắc lỗi. Thêm vào đó, các tiêu chuẩn và trình biên dịch C hiện đại ngày càng cung cấp các hàm tích hợp và các phương án an toàn hơn cho thao tác chuỗi, nhằm giảm thiểu các sự cố thường thấy trong các cơ sở mã C cũ. Tuy nhiên, sự hiểu biết cơ bản về xử lý văn bản thủ công vẫn là một kỹ năng quý báu trong bộ công cụ của lập trình viên, đặc biệt là để tối ưu hóa các ứng dụng quan trọng về hiệu suất.
