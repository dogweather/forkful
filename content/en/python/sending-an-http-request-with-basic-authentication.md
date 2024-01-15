---
title:                "Sending an http request with basic authentication"
html_title:           "Python recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

Sending an HTTP request with basic authentication is necessary when accessing resources that require user authentication, such as private APIs or web applications protected by a login page. It allows users to securely access these resources by verifying their identity using a username and password.

## How To

To send an HTTP request with basic authentication in Python, you will need to use the `requests` library. First, import the library by typing `import requests` at the top of your Python file.

Next, you will need to specify the URL of the resource you want to access and create a `requests.get()` call with the URL as the argument. For example, if we wanted to access the GitHub API, we would use the following code:

```
import requests

url = "https://api.github.com/user"
response = requests.get(url)
```

Now, we need to add the basic authentication credentials to our request. To do this, we will use the `auth` parameter and pass in a tuple with the username and password. The code should look like this:

```
import requests

url = "https://api.github.com/user"
response = requests.get(url, auth=("username", "password"))
```

Finally, we can print out the response to see the result of our request:

```
import requests

url = "https://api.github.com/user"
response = requests.get(url, auth=("username", "password"))

print(response.text)
```

The output will be a JSON string containing information about the authenticated user.

```
{
   "login": "username",
   "id": 1,
   "node_id": "MDQ6VXNlcjE=",
   "avatar_url": "https://avatars.githubusercontent.com/u/1?v=4",
   "gravatar_id": "",
   "url": "https://api.github.com/users/username",
   "html_url": "https://github.com/username",
   "followers_url": "https://api.github.com/users/username/followers",
   "following_url": "https://api.github.com/users/username/following{/other_user}",
   "gists_url": "https://api.github.com/users/username/gists{/gist_id}",
   "starred_url": "https://api.github.com/users/username/starred{/owner}{/repo}",
   "subscriptions_url": "https://api.github.com/users/username/subscriptions",
   "organizations_url": "https://api.github.com/users/username/orgs",
   "repos_url": "https://api.github.com/users/username/repos",
   "events_url": "https://api.github.com/users/username/events{/privacy}",
   "received_events_url": "https://api.github.com/users/username/received_events",
   "type": "User",
   "site_admin": false,
   "name": "Username",
   "company": null,
   "blog": "",
   "location": null,
   "email": null,
   "hireable": null,
   "bio": null,
   "twitter_username": null,
   "public_repos": 1,
   "public_gists": 0,
   "followers": 0,
   "following": 0,
   "created_at": "2008-01-14T04:33:35Z",
   "updated_at": "2009-01-24T04:56:27Z"
}
```

## Deep Dive

In the previous example, we used the `auth` parameter to pass in a tuple with the username and password. However, we can also use the `HTTPBasicAuth` class from the `requests` library to achieve the same result.

Instead of passing a tuple to the `auth` parameter, we can create an instance of the `HTTPBasicAuth` class and pass in the username and password as arguments. The code would look like this:

```
import requests
from requests.auth import HTTPBasicAuth

url = "https://api.github.com/user"
response = requests.get(url, auth=HTTPBasicAuth("username", "password"))

print(response.text)
```

Both methods work the same way, so you can choose whichever one you feel more comfortable with.

## See Also

- [Requests library documentation](https://docs.python-requests.org/en/master/)
- [HTTP authentication explained](https://www.freecodecamp.org/news/http-authentication-explained/)