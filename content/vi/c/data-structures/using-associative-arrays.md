---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:06.177835-07:00
description: "M\u1EA3ng li\xEAn k\u1EBFt, \u0111\u01B0\u1EE3c bi\u1EBFt \u0111\u1EBF\
  n trong c\xE1c ng\xF4n ng\u1EEF kh\xE1c nh\u01B0 b\u1EA3n \u0111\u1ED3 ho\u1EB7\
  c t\u1EEB \u0111i\u1EC3n, l\xE0 c\xE1c c\u1EB7p kh\xF3a-gi\xE1 tr\u1ECB \u0111\u01B0\
  \u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 tra c\u1EE9u v\xE0 thao t\xE1c d\u1EEF li\u1EC7\
  u m\u1ED9t c\xE1ch\u2026"
lastmod: '2024-02-25T18:49:35.605571-07:00'
model: gpt-4-0125-preview
summary: "M\u1EA3ng li\xEAn k\u1EBFt, \u0111\u01B0\u1EE3c bi\u1EBFt \u0111\u1EBFn\
  \ trong c\xE1c ng\xF4n ng\u1EEF kh\xE1c nh\u01B0 b\u1EA3n \u0111\u1ED3 ho\u1EB7\
  c t\u1EEB \u0111i\u1EC3n, l\xE0 c\xE1c c\u1EB7p kh\xF3a-gi\xE1 tr\u1ECB \u0111\u01B0\
  \u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 tra c\u1EE9u v\xE0 thao t\xE1c d\u1EEF li\u1EC7\
  u m\u1ED9t c\xE1ch\u2026"
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Mảng liên kết, được biết đến trong các ngôn ngữ khác như bản đồ hoặc từ điển, là các cặp khóa-giá trị được sử dụng để tra cứu và thao tác dữ liệu một cách hiệu quả. Khác với mảng truyền thống sử dụng chỉ số nguyên, mảng liên kết sử dụng khóa, làm cho việc truy cập dữ liệu trở nên trực quan và linh hoạt hơn cho lập trình viên.

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
