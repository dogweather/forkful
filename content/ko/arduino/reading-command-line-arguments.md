---
title:    "Arduino: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## 왜

아두이노 프로그래밍은 다양한 코딩 기술을 배워야하기 때문에 간혹 복잡하고 어려울 수 있습니다. 하지만 이러한 코딩 기술 중 하나인 커맨드 라인 인수 읽기는 아두이노 프로그래밍의 중요한 부분이며 프로그래밍을 더욱 효과적으로 만들어 줍니다. 따라서 커맨드 라인 인수 읽기에 대해 배우는 것은 아두이노 프로그래밍을 더욱 재미있게 만드는데 도움이 됩니다.

## 한 번에 알아보는 방법

커맨드 라인 인수를 읽는 가장 간단한 방법은 ```Arduino``` 명령어에 ```String[]``` 배열을 매개변수로 전달하는 것입니다. 예를 들면, 다음 코드를 실행해보세요.

```Arduino
void setup() {
    Serial.begin(9600);
    while(!Serial) {}
    String[] args = {"arg1", "arg2", "arg3"};
    Serial.println(args[0]);
    Serial.println(args[1]);
    Serial.println(args[2]);
}

void loop() {
  
}
```

위의 코드를 실행하면 아래와 같은 결과를 볼 수 있습니다.

```
arg1
arg2
arg3
```

직접 코드를 작성하여 커맨드 라인 인수를 입력할 수도 있지만, 좀 더 효율적인 방법은 시리얼 모니터를 사용하여 커맨드 라인 인수를 입력하는 것입니다. 시리얼 모니터로 입력한 커맨드 라인 인수는 Serial 변수를 통해 읽을 수 있습니다.

```Arduino
#include <String.h>
String[] args = {"arg1", "arg2", "arg3"};

void setup() {
  Serial.begin(9600);
  while(!Serial) {}
  if(Serial.available() > 0) {
    String inputArgs = Serial.readStringUntil('\n');
    args = inputArgs.split(',');
    for(int i = 0; i < args.length; i++) {
      Serial.println(args[i]);
    }
  }
}

void loop() {
  
}
```

위의 코드를 실행하고 시리얼 모니터에 ```arg1,arg2,arg3```를 입력해보세요. 입력한 커맨드 라인 인수가 배열로 출력되는 것을 확인할 수 있습니다.

## 더 깊이 들어가보기

커맨드 라인 인수를 읽기 위해 사용할 수 있는 여러 가지 함수와 방법이 있습니다. 자세한 내용은 [Arduino 공식 웹사이트](https://www.arduino.cc/reference/en/language/functions/communication/serial/)를 참조하세요. 또한 커맨드 라인 인수를 활용한 다양한 예제 코드를 [Github](https://github.com/search?q=arduino+command+line+arguments)에서 찾아볼 수 있습니다.

## 관련 링크

- [Arduino 공식 웹사이트](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [GitHub에서 커맨드 라인 인수 관련 예제 코드 검색](https://github.com/search?q=arduino+command+line+arguments&type=Repositories)