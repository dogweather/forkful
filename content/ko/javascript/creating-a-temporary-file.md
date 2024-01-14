---
title:    "Javascript: 임시 파일 만들기"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## 왜

Temporary(임시적인) 파일을 생성하는 작업은 개발자에게 매우 중요합니다. 임시 파일은 보조적인 데이터를 저장하거나 사용 후에 삭제할 필요가 있는 파일을 관리하기에 효율적인 방법입니다. 또한, 임시 파일을 생성하는 것은 애플리케이션의 성능을 향상시키는 데도 중요합니다. 

## 만드는 방법

임시 파일을 만드는 가장 간단한 방법은 Node.js의 `temp` 모듈을 사용하는 것입니다. 먼저, 다음과 같이 모듈을 설치합니다.

```Javascript
npm install temp
```

다음으로, `temp` 모듈을 사용하여 임시 파일을 만드는 방법을 알아보겠습니다.

```Javascript
var temp = require('temp');
var fs = require('fs');

temp.open('myprefix', function(err, info) {
    // 임시 파일 경로와 파일 디스크립터를 얻습니다.
    if (!err) {
        // 임시 파일에 쓰기 작업을 수행합니다.
        fs.writeSync(info.fd, 'Hello World!');
        // 임시 파일을 닫고, 자동으로 삭제됩니다.
        temp.cleanupSync();
    }
});
```

위의 예시에서는 `temp` 모듈의 `open` 메소드를 사용하여 임시 파일을 열고 경로와 파일 디스크립터를 얻습니다. `fs` 모듈을 사용하여 임시 파일에 데이터를 쓰고, `cleanupSync()` 메소드를 사용하여 임시 파일을 닫고 삭제합니다. 이러한 방법으로 임시 파일을 만들 수 있습니다.

## 깊게 파고들기

위의 예시에서 사용한 `temp` 모듈의 `open` 메소드는 여러 가지 옵션을 제공합니다. 예를 들어, `prefix` 옵션을 사용하여 임시 파일의 이름을 지정할 수 있고, `dir` 옵션을 사용하여 임시 파일의 위치를 지정할 수 있습니다. 또한, `unlink` 옵션을 사용하여 `temp` 모듈이 임시 파일을 삭제하는 것을 막을 수 있습니다.

또 다른 방법으로는, `tmp.tmpName()` 메소드를 사용하여 임시 파일의 경로를 얻어낼 수도 있습니다.

```Javascript
tmp.tmpName({ prefix: 'myprefix' }, function(err, path) {
    if (!err) {
        // 임시 파일의 경로를 얻어내 사용합니다.
    }
});
```

더 많은 옵션은 공식 문서에서 확인할 수 있습니다. [https://github.com/bruce/node-temp](https://github.com/bruce/node-temp)

## See Also

- [Node.js 공식 문서 - 버퍼 기본 사용법](https://nodejs.org/api/buffer.html)
- [fs 모듈 - 파일 시스템 접근하기](https://nodejs.org/api/fs.html)
- [temp 모듈 - 임시 파일 생성하기](https://github.com/bruce/node-temp)