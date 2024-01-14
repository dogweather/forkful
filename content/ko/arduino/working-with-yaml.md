---
title:                "Arduino: yaml 작업하기"
simple_title:         "yaml 작업하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜 YAML을 활용해야 할까
 아두이노 프로그래밍에서 YAML을 활용하는 것은 프로젝트를 더 쉽고 효율적으로 관리할 수 있도록 도와줍니다. 이 데이터 포맷은 코드를 더 읽기 쉽고 유지보수하기 쉽게 만들어줍니다. 그리고 YAML 파일은 프로젝트 설정 파일이나 데이터베이스를 저장하는데에도 유용하게 사용될 수 있습니다.

## 어떻게 YAML을 활용할까
아래는 YAML을 활용한 아두이노 코딩 예제와 그 결과물입니다.

```Arduino

#include <SPI.h>
#include <Ethernet.h>
#include <YAML.h>

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
byte server[] = { 192, 168, 1, 10 }; // YAML 파일에서 읽어온 서버 IP 주소

EthernetClient client;

void setup()
{
  Ethernet.begin(mac);
  Serial.begin(9600);
  
  if (client.connect(server, 80)) {
    YAML::Node config = YAML::LoadFile("config.yaml"); // config.yaml 파일에서 정보를 읽어옴
    String message = config["message"].as<String>(); // 메시지를 String 형태로 변환
    client.println(message); // 서버로 메시지 전송
  } else {
    Serial.println("connection failed");
  }
}

void loop()
{
  if (client.available()) {
    char c = client.read();
    Serial.print(c);
  }
}
```

위 예제에서는 YAML 라이브러리를 이용해 config.yaml 파일에서 서버 IP 주소를 읽어온 뒤 해당 서버로 메시지를 보내는 코드를 작성하였습니다. 이와 같이 YAML 파일을 활용하면 설정 값들을 코드에 넣어주는 번거로움을 줄일 수 있습니다.

## YAML 작업의 깊은 부분
YAML은 다른 프로그래밍 언어처럼 변수나 조건문 등을 사용할 수 없기 때문에 초보자들은 처음에 적응하기 어려울 수 있습니다. 또한 YAML파일에서의 들여쓰기와 공백의 사용에도 주의를 기울여야 합니다. 하지만 한번 익숙해지면 프로젝트 관리나 설정 값 변경 등에 매우 유용한 형식이 될 수 있습니다.

## 자세히 보기
- YAML 라이브러리 공식 홈페이지: https://github.com/jbeder/yaml-cpp
- YAML 데이터 포맷에 대한 자세한 설명: https://yaml.org/
- 아두이노에서 YAML 파일 읽어오기 예제: https://www.hackster.io/iehoon/read-a-yaml-file-f4102b

# 더 알아보기
- 아두이노 라이브러리 다운로드하기: https://www.arduino.cc/en/guide/libraries
- 아두이노 공식 사이트: https://www.arduino.cc/