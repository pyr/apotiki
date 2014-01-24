var app = angular.module('repo', ['ngRoute']);

app.controller('Apotiki', function($scope, $routeParams, $location, $http) {

    $scope.repolist = [];
    $scope.repo = {};

    $scope.refresh = function () {
        $http.get('/repo').success(function (data) {
            $scope.repo = data;
            for (var k in data) {
                for (var subk in data[k]) {
                    $scope.repolist.push(data[k][subk]);
                }
            }
        });
    };

    if ($routeParams.name) { $scope.name = $routeParams.name; }
    if ($routeParams.arch) { $scope.arch = $routeParams.arch; }

    $scope.refresh();
});

app.config(function($routeProvider) {
    $routeProvider
        .when('/repo', {templateUrl: 'listing.html', controller: 'Apotiki'})
        .when('/repo/:arch/:name', {templateUrl: 'details.html', controller: 'Apotiki'})
        .when('/post', {templateUrl: 'post.html'})
        .otherwise({redirectTo: '/repo'});
});
