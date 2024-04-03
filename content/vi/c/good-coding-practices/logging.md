---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:33.515194-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong C, ghi log c\xF3 th\u1EC3 \u0111\u01B0\
  \u1EE3c th\u1EF1c hi\u1EC7n v\u1EDBi c\xE1c thao t\xE1c t\u1EC7p c\u01A1 b\u1EA3\
  n ho\u1EB7c s\u1EED d\u1EE5ng c\xE1c th\u01B0 vi\u1EC7n ph\u1EE9c t\u1EA1p h\u01A1\
  n. \u0110\u1EC3 \u0111\u01A1n gi\u1EA3n, ch\xFAng ta s\u1EBD b\u1EAFt \u0111\u1EA7\
  u\u2026"
lastmod: '2024-03-13T22:44:37.276945-06:00'
model: gpt-4-0125-preview
summary: "Trong C, ghi log c\xF3 th\u1EC3 \u0111\u01B0\u1EE3c th\u1EF1c hi\u1EC7n\
  \ v\u1EDBi c\xE1c thao t\xE1c t\u1EC7p c\u01A1 b\u1EA3n ho\u1EB7c s\u1EED d\u1EE5\
  ng c\xE1c th\u01B0 vi\u1EC7n ph\u1EE9c t\u1EA1p h\u01A1n."
title: "Ghi nh\u1EADt k\xFD"
weight: 17
---

## Làm thế nào:
Trong C, ghi log có thể được thực hiện với các thao tác tệp cơ bản hoặc sử dụng các thư viện phức tạp hơn. Để đơn giản, chúng ta sẽ bắt đầu với thư viện I/O tiêu chuẩn. Các đoạn mã sau đây trình bày cách triển khai ghi log cơ bản.

Để ghi các thông điệp đơn giản:

```c
#include <stdio.h>

int main() {
    FILE *logFile;
    logFile = fopen("application.log", "a"); // Mở tệp log ở chế độ bổ sung
    
    if (logFile == NULL) {
        perror("Lỗi khi mở tệp log.");
        return -1;
    }
    
    fprintf(logFile, "Khởi động ứng dụng.\n");
    
    // Logic ứng dụng của bạn ở đây
    
    fprintf(logFile, "Ứng dụng kết thúc thành công.\n");
    fclose(logFile);
    
    return 0;
}
```

Kết quả trong `application.log`:

```
Khởi động ứng dụng.
Ứng dụng kết thúc thành công.
```

Để bao gồm các log chi tiết hơn với dấu thời gian và mức độ log:

```c
#include <stdio.h>
#include <time.h>

void logMessage(FILE *logFile, const char* level, const char* message) {
    time_t now;
    time(&now);
    char* datetime = ctime(&now);
    datetime[strlen(datetime)-1] = '\0'; // Loại bỏ ký tự xuống dòng
    fprintf(logFile, "[%s] %s - %s\n", datetime, level, message);
}

int main() {
    FILE *logFile;
    logFile = fopen("detailed.log", "a");
    
    if (logFile == NULL) {
        perror("Lỗi khi mở tệp log.");
        return -1;
    }
    
    logMessage(logFile, "INFO", "Ứng dụng bắt đầu");
    // Logic ứng dụng của bạn ở đây
    logMessage(logFile, "ERROR", "Một ví dụ lỗi");
    
    fclose(logFile);
    
    return 0;
}
```

Kết quả trong `detailed.log`:

```
[Thu Mar 10 14:32:01 2023] INFO - Ứng dụng bắt đầu
[Thu Mar 10 14:32:02 2023] ERROR - Một ví dụ lỗi
```

## Tìm hiểu sâu hơn
Ghi log trong C, như đã trình bày, dựa vào các thao tác tệp đơn giản, đây là cách làm hiệu quả nhưng không mạnh mẽ hay linh hoạt như các tiện ích ghi log trong các ngôn ngữ khác, như module `logging` của Python hay `Log4j` của Java. Để có khả năng ghi log nâng cao hơn trong C, lập trình viên thường chuyển sang sử dụng các thư viện như `syslog` trên các hệ thống giống Unix, cung cấp quản lý log toàn hệ thống, hoặc các thư viện bên thứ ba như `log4c`.

Lịch sử, ghi log là một phần không thể tách rời của lập trình, xuất phát từ những thực hành lập trình đầu tiên khi việc theo dõi và hiểu dòng chảy và lỗi của chương trình chủ yếu được thực hiện thông qua in ấn vật lý. Khi hệ thống phát triển, ghi log trở nên tinh vi hơn, hiện hỗ trợ nhiều mức độ nghiêm trọng khác nhau, quay vòng log và ghi log không đồng bộ.

Mặc dù thư viện tiêu chuẩn của C cung cấp các công cụ cơ bản để thực hiện ghi log, giới hạn của nó thường dẫn đến việc tạo ra các khung ghi log tùy chỉnh hoặc sử dụng các thư viện bên ngoài cho các giải pháp ghi log phong phú và linh hoạt hơn. Mặc dù có những hạn chế này, việc hiểu và thực hiện ghi log cơ bản trong C là rất quan trọng cho việc gỡ lỗi và bảo trì phần mềm, đặc biệt là trong môi trường nơi mà sự phụ thuộc bên ngoài cần được giảm thiểu.
