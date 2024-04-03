---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:17.856357-06:00
description: "TOML (Tom's Obvious, Minimal Language) \u0E40\u0E1B\u0E47\u0E19\u0E23\
  \u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\u0E01\u0E33\
  \u0E2B\u0E19\u0E14\u0E04\u0E48\u0E32\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\u0E07\
  \u0E48\u0E32\u0E22\u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E32\u0E01\u0E21\u0E35\
  \u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E20\u0E32\u0E29\u0E32\u0E17\u0E35\u0E48\u0E0A\
  \u0E31\u0E14\u0E40\u0E08\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.709038-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language) \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\
  \u0E19\u0E14\u0E04\u0E48\u0E32\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\u0E07\u0E48\
  \u0E32\u0E22\u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E32\u0E01\u0E21\u0E35\u0E01\
  \u0E32\u0E23\u0E43\u0E0A\u0E49\u0E20\u0E32\u0E29\u0E32\u0E17\u0E35\u0E48\u0E0A\u0E31\
  \u0E14\u0E40\u0E08\u0E19 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\
  \u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E04\u0E48\
  \u0E32\u0E43\u0E19\u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19\
  \u0E40\u0E1E\u0E23\u0E32\u0E30\u0E04\u0E27\u0E32\u0E21\u0E40\u0E23\u0E35\u0E22\u0E1A\
  \u0E07\u0E48\u0E32\u0E22\u0E41\u0E25\u0E30\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E43\u0E19\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E14\
  \u0E49\u0E42\u0E14\u0E22\u0E04\u0E19\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E21\u0E31\u0E19\
  \u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E40\u0E25\u0E37\u0E2D\u0E01\u0E17\u0E35\
  \u0E48\u0E14\u0E35\u0E40\u0E22\u0E35\u0E48\u0E22\u0E21\u0E21\u0E32\u0E01\u0E01\u0E27\
  \u0E48\u0E32\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E2D\u0E37\u0E48\u0E19 \u0E46\
  \ \u0E40\u0E0A\u0E48\u0E19 XML \u0E2B\u0E23\u0E37\u0E2D JSON \u0E43\u0E19\u0E1A\u0E32\
  \u0E07\u0E1A\u0E23\u0E34\u0E1A\u0E17."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A TOML"
weight: 39
---

## วิธีการ:
เพื่อที่จะทำงานกับ TOML ในภาษา C คุณจำเป็นต้องมีไลบรารีที่สามารถแยกวิเคราะห์ไฟล์ TOML ได้ เนื่องจากไลบรารีมาตรฐานของ C ไม่รวมฟังก์ชันนี้ไว้ ตัวเลือกที่ได้รับความนิยมคือ `tomlc99`, ตัวแยกวิเคราะห์ TOML ที่มีน้ำหนักเบาสำหรับ C99 นี่คือคู่มือแบบย่อในการอ่านไฟล์การกำหนดค่า TOML อย่างง่าย:

ขั้นแรก ตรวจสอบให้แน่ใจว่าคุณติดตั้ง `tomlc99` และเชื่อมโยงกับโปรเจ็กต์ของคุณอย่างเหมาะสม

**ไฟล์ TOML ตัวอย่าง (`config.toml`):**
```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

**โค้ด C ในการแยกวิเคราะห์ไฟล์นี้:**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *configFile;
    configFile = fopen("config.toml", "r");
    if (!configFile) {
        perror("Cannot open file");
        return EXIT_FAILURE;
    }

    toml_table_t *config = toml_parse_file(configFile, NULL, 0);
    if (!config) {
        fprintf(stderr, "Error parsing file\n");
        fclose(configFile);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(config, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("Database Server: %s\n", server);

        toml_array_t *ports = toml_array_in(database, "ports");
        for (int i = 0; i < toml_array_nelem(ports); i++) {
            int64_t port;
            toml_int_at(ports, i, &port);
            printf("Port %d: %ld\n", i, port);
        }
    }

    toml_free(config);
    fclose(configFile);
    return EXIT_SUCCESS;
}
```

**ผลลัพธ์:**
```
Database Server: "192.168.1.1"
Port 0: 8001
Port 1: 8001
Port 2: 8002
```

## ศึกษาลึก
TOML ถูกสร้างขึ้นโดย Tom Preston-Werner ผู้ร่วมก่อตั้ง GitHub เป็นการตอบสนองต่อข้อจำกัดที่เขารับรู้ในรูปแบบไฟล์การกำหนดค่าอื่นๆ จุดประสงค์คือการทำให้ตรงไปตรงมาและไม่มีความกำกวม ทั้งสำหรับมนุษย์และคอมพิวเตอร์ในการอ่านและเขียนโดยไม่ต้องการกฎการแยกวิเคราะห์ที่ซับซ้อน ในระบบนิเวศของภาษา C, TOML ไม่ได้ถือเป็นพลเมืองระดับหนึ่งเหมือนที่อาจพบในภาษาระดับสูงเช่น Rust กับ `serde_toml` หรือ Python กับ `toml` ซึ่งมีไลบรารีที่ให้การสนับสนุนเนื้อเน็บ เน้นไปที่ความเรียบง่ายและประสิทธิภาพ

แม้ว่า TOML จะได้รับการชื่นชมสำหรับความชัดเจนของมัน แต่เมื่อเลือกรูปแบบไฟล์การกำหนดค่า สิ่งสำคัญคือต้องพิจารณาถึงความต้องการของโปรเจ็กต์ ในสถานการณ์ที่ต้องการโครงสร้างที่ซับซ้อนมากขึ้นหรือต้องการการทำงานร่วมกับเว็บ API  JSON หรือแม้กระทั่ง YAML อาจเสนอการเข้ากันได้ดีกว่าโดยมีความซับซ้อนเพิ่มขึ้น TOML โดดเด่นในการกำหนดค่าที่ความอ่านง่ายและความเรียบง่ายเป็นสิ่งสำคัญ ไม่จำเป็นต้องเป็นที่ที่โครงสร้างข้อมูลที่ทันสมัยที่สุดจำเป็น
