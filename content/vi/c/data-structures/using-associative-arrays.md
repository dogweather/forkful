---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:06.177835-07:00
description: "L\xE0m th\u1EBF n\xE0o: C kh\xF4ng c\xF3 h\u1ED7 tr\u1EE3 t\xEDch h\u1EE3\
  p s\u1EB5n cho m\u1EA3ng li\xEAn k\u1EBFt nh\u01B0 m\u1ED9t s\u1ED1 ng\xF4n ng\u1EEF\
  \ c\u1EA5p cao, nh\u01B0ng b\u1EA1n c\xF3 th\u1EC3 m\xF4 ph\u1ECFng ch\xFAng s\u1EED\
  \ d\u1EE5ng c\u1EA5u tr\xFAc v\xE0 b\u0103m. D\u01B0\u1EDBi\u2026"
lastmod: '2024-03-13T22:44:37.258031-06:00'
model: gpt-4-0125-preview
summary: "C kh\xF4ng c\xF3 h\u1ED7 tr\u1EE3 t\xEDch h\u1EE3p s\u1EB5n cho m\u1EA3\
  ng li\xEAn k\u1EBFt nh\u01B0 m\u1ED9t s\u1ED1 ng\xF4n ng\u1EEF c\u1EA5p cao, nh\u01B0\
  ng b\u1EA1n c\xF3 th\u1EC3 m\xF4 ph\u1ECFng ch\xFAng s\u1EED d\u1EE5ng c\u1EA5u\
  \ tr\xFAc v\xE0 b\u0103m."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

## Làm thế nào:
C không có hỗ trợ tích hợp sẵn cho mảng liên kết như một số ngôn ngữ cấp cao, nhưng bạn có thể mô phỏng chúng sử dụng cấu trúc và băm. Dưới đây là một ví dụ đơn giản sử dụng sự kết hợp của một struct và một hàm băm đơn giản để triển khai một mảng liên kết để lưu trữ và truy cập số nguyên bằng khóa chuỗi.

Đầu tiên, định nghĩa một cấu trúc để đại diện cho một cặp khóa-giá trị và một cái khác để đại diện cho chính mảng liên kết:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TABLE_SIZE 128

typedef struct {
    char* key;
    int value;
} KeyValuePair;

typedef struct {
    KeyValuePair* items[TABLE_SIZE];
} AssocArray;

unsigned int hash(char* key) {
    unsigned long int value = 0;
    unsigned int i = 0;
    unsigned int key_len = strlen(key);

    for (; i < key_len; ++i) {
        value = value * 37 + key[i];
    }

    value = value % TABLE_SIZE;

    return value;
}

void initArray(AssocArray* array) {
    for (int i = 0; i < TABLE_SIZE; ++i) {
        array->items[i] = NULL;
    }
}

void insert(AssocArray* array, char* key, int value) {
    unsigned int slot = hash(key);

    KeyValuePair* item = (KeyValuePair*)malloc(sizeof(KeyValuePair));
    item->key = strdup(key);
    item->value = value;

    array->items[slot] = item;
}

int find(AssocArray* array, char* key) {
    unsigned int slot = hash(key);

    if (array->items[slot]) {
        return array->items[slot]->value;
    }
    return -1;
}

int main() {
    AssocArray a;
    initArray(&a);

    insert(&a, "key1", 1);
    insert(&a, "key2", 2);

    printf("%d\n", find(&a, "key1")); // Đầu ra: 1
    printf("%d\n", find(&a, "key2")); // Đầu ra: 2

    return 0;
}
```

Ví dụ này biểu diễn các thao tác cơ bản: khởi tạo một mảng liên kết, chèn vào các cặp khóa-giá trị và tìm kiếm giá trị bằng khóa. Lưu ý rằng code này thiếu xử lý va chạm và chỉ nhằm mục đích giáo dục.

## Sâu hơn
Khái niệm về mảng liên kết tồn tại trước C, nhưng bản chất cấp thấp của ngôn ngữ không trực tiếp hỗ trợ chúng như các kiểu tích hợp sẵn. Điều này khuyến khích một sự hiểu biết sâu sắc hơn về cấu trúc dữ liệu và thuật toán, bao gồm cơ chế băm để ánh xạ khóa-giá trị một cách hiệu quả. Nhiều thư viện và khung làm việc C cung cấp các cách tiếp cận tinh vi hơn cho việc triển khai mảng liên kết, như `GHashTable` của GLib, cung cấp một thực hiện mạnh mẽ hoàn chỉnh với xử lý va chạm, thay đổi kích thước động và hỗ trợ các loại khóa và giá trị tùy ý.

Mặc dù việc xây dựng mảng liên kết bằng tay trong C có thể được coi là cồng kềnh so với các ngôn ngữ có hỗ trợ tích hợp, nhưng nó cung cấp những hiểu biết quý giá về cách hoạt động bên trong của cấu trúc dữ liệu, tăng cường kỹ năng giải quyết vấn đề và tối ưu hóa của lập trình viên. Tuy nhiên, đối với mã nguồn sản phẩm hoặc các ứng dụng phức tạp hơn, việc sử dụng các thư viện hiện có như GLib thường là cách tiếp cận thực tế và tiết kiệm thời gian hơn.
