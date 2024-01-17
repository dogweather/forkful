---
title:                "yaml 작업하기"
html_title:           "Arduino: yaml 작업하기"
simple_title:         "yaml 작업하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

# 어떤것과 왜?

YAML 프로그래밍은 이제 아두이노에서 사용할 수 있습니다. 그렇게 하기전에, 우리는 YAML에 대해 두 가지만 알고 있으면 됩니다. 첫째, YAML은 데이터 직렬화 양식입니다. 즉, 데이터를 파일에 저장하거나 네트워크를 통해 전송하기위해 사용됩니다. 둘째, 프로그래머는 YAML을 사용하여 데이터를 구조화하고 읽고 쓰기 위해 사용합니다.

# 어떻게:

```
#include <YAML.h>

// YAML 형식으로 데이터를 읽고 쓰는 예제
void setup() {
  YAML::Node node;
  node["name"] = "Arduino";
  node["version"] = "1.8.13";
  node["platform"] = "AVR";
  
  // YAML 파일로 저장하기
  node["data"]["sensor1"] = 100;
  node["data"]["sensor2"] = 200;
  File file = SD.open("data.yaml", FILE_WRITE);
  YAML::Emitter emitter(file);
  emitter << node;
  
  // YAML 파일에서 읽어오기
  YAML::Node readNode;
  File readFile = SD.open("data.yaml");
  YAML::Parser parser(readFile);
  parser >> readNode;
  
  // 읽어온 데이터 출력하기
  int sensor1 = readNode["data"]["sensor1"].as<int>();
  int sensor2 = readNode["data"]["sensor2"].as<int>();
  
  // 시리얼 모니터에 출력하기
  Serial.begin(9600);
  Serial.print("sensor1: ");
  Serial.println(sensor1);
  Serial.print("sensor2: ");
  Serial.println(sensor2);
}
```

# 딥 다이브:

(1) YAML은 2000년에 개발된 형식이며, 이전에는 XML이 일반적으로 사용되었습니다. XML에 비해 YAML은 구조적으로 더 간단하고 읽기 쉽기 때문에 많은 프로그래머들이 채택하게 되었습니다. (2) 프로그래머들은 JSON이나 CSV와 같은 다른 데이터 형식을 사용할 수도 있지만, YAML은 보다 유연하고 효율적인 데이터 구조를 제공하므로 선호합니다. (3) 아두이노에서 YAML을 사용하기 위해 <YAML.h> 라이브러리를 설치해야 합니다. 이 라이브러리는 ESP32, ESP8266, AVR 등 다양한 아두이노 기반 보드에서 사용 가능합니다.

# 참고자료:

- <YAML.h> 라이브러리 설치 가이드: https://github.com/arduino-libraries/YAML/blob/master/README.md
- YAML에 관한 더 자세한 정보: https://yaml.org/
- YAML 문법 가이드: https://docs.ansible.com/ansible/latest/reference_appendices/YAMLSyntax.html