var gulp = require('gulp');
var elm  = require('gulp-elm');
var plugins = require('gulp-load-plugins')({lazy:false});
var del = require('del');
var runSequence = require('run-sequence');

gulp.task('elm-init', elm.init);

gulp.task('elm', ['elm-init'], function(){
  return gulp.src('src/**/*.elm')
  .pipe(elm())
  .pipe(gulp.dest('build'));
});

gulp.task('sass', function () {
  'use strict';
  return gulp.src('./src/**/*.scss')
  .pipe(plugins.sass())
  .pipe(plugins.concat('style.css'))
  .pipe(gulp.dest('./build'));
});

gulp.task('index', function () {
  'use strict';
  gulp.src(['./src/index.html'], {'base': './src'})
  .pipe(gulp.dest('./build/'));
});

gulp.task('connect', function(){
  plugins.connect.server({
    root: ['build','.'],
    port: 9000,
    livereload: true
  });
});

gulp.task('watch',function(){
  'use strict';
  gulp.watch([
    '**/*.html',
    '**/*.js',
    '**/*.css'
  ], function(event) {
    return gulp.src(event.path)
    .pipe(plugins.connect.reload());
  });
  gulp.watch('./src/index.html',['index']);
  gulp.watch('./src/**/*.elm', ['elm']);
  gulp.watch(['src/**/*.scss'],['sass']);
});

gulp.task('clean', function(cb){
  'use strict';
  del(['build'], cb);
});

gulp.task('default', function(cb){
  runSequence('clean', ['connect','sass', 'elm', 'index','watch'], cb);
});
