---
title:                "Clojure: 发送带有基本认证的http请求"
simple_title:         "发送带有基本认证的http请求"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 为什么
发送HTTP请求是构建Web应用程序的重要组成部分。使用基本认证可以确保只有授权的用户才能访问保护的资源。 

# 怎么做
使用Clojure发送带有基本认证的HTTP请求非常简单。首先，我们需要导入Clojure的http.client库，它包含了发送HTTP请求所需的功能。然后，我们可以使用`client/post`函数来发送POST请求，并传递URL和请求体作为参数。最后，我们可以通过传递用户名和密码来实现基本认证。下面是一个示例代码：
```
(ns http.basic-auth
  (:require [clojure.core.async :refer [<!]]
            [clj-http.client :as client]))

(let [url "https://www.example.com/api/login"
      body {:username "user" :password "pass"}]
  (<! (client/post url {:form-params body
                         :basic-auth ["user", "pass"]})))
```
发送请求后，我们将会收到一个响应，它可能包含许多信息，但通常我们只需要关注响应的状态码（如200表示成功，401表示未授权）和响应体。可以通过`status`和`body`关键字来访问这些信息。下面是一个处理响应的例子：
```
(doseq [response (<! (client/post url {:form-params body
                                       :basic-auth ["user" "pass"]}))]
  (println "Status:" (get response :status))
  (println "Body:" (get response :body)))
```

# 深入探讨
当我们发送带有基本认证的HTTP请求时，Clojure会自动为我们生成`Authorization`头部，它包含用户名和密码的Base64编码字符串。但是，有时候我们可能需要手动添加其他头部和头部值。这可以通过传递`:headers`关键字来实现，如下所示：
```
(client/post url {:form-params body
                   :basic-auth ["user", "pass"]
                   :headers {"Content-Type" "application/json"
                             "User-Agent" "MyApp/1.0"}})
```

# 参考资料
- https://github.com/dakrone/clj-http
- https://stackoverflow.com/questions/291397/clojure-how-to-send-an-http-get-request-with-ant-http