---
title:    "Arduino: 디렉토리가 존재하는지 확인하기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜 이것을 하는가?

디렉터리가 존재하는지 확인하는 것은 매우 유용한 기능입니다. 디렉터리가 없으면 프로그램이 오류를 방지하고 미리 대비할 수 있도록 알려줍니다.

## 진행 방법

우선 디렉터리가 존재하는지 확인하기 위해 `File` 라이브러리를 불러와야 합니다. 그 후에 `exists` 함수를 사용하여 디렉터리가 존재하는지 확인할 수 있습니다. 아래의 코드를 참고해주세요.

```Arduino
#include <File.h>

void setup() {
  Serial.begin(9600); // 시리얼 통신 속도 설정
  if (SD.exists("directory")) { // 디렉터리가 존재하는지 확인
    Serial.println("디렉터리가 존재합니다.");
  } else {
    Serial.println("디렉터리가 존재하지 않습니다.");
  }
}

void loop() {

}
```

위의 코드를 실행하면, 시리얼 모니터에 디렉터리가 존재하는지 여부를 확인할 수 있습니다. 존재하면 `디렉터리가 존재합니다.`라는 문구가 나오고, 존재하지 않으면 `디렉터리가 존재하지 않습니다.`라는 문구가 나옵니다.

## 더 깊이 들어가기

디렉터리의 존재 여부를 확인하는 것 이외에도 `File` 라이브러리에는 다양한 기능이 있습니다. 예를 들어, 디렉터리를 생성하거나 삭제할 수도 있고, 디렉터리에 있는 파일들의 이름을 확인할 수도 있습니다. 더 자세한 내용은 아래의 링크를 참고해주세요.

## 더 알아보기

- [Arduino 공식 문서 - SD 라이브러리](https://www.arduino.cc/en/Reference/SD)
- [Arduino 프로젝트 - SD 라이브러리를 이용한 파일 탐색기](https://create.arduino.cc/projecthub/abhi.dangare/sdio-card-reader-2389f8)